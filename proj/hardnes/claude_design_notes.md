
# NES FPGA Emulator Design Document

(this was my first fpga project so chatted with claude a lot about all the
design primitives/io driving/block ram and stuff -- this is its summary)

## Overview

Cycle-accurate NES emulator targeting Nexys A7-100T (Artix-7 XC7A100T).

**Modes:**
- **Dev mode:** Ethernet for ROM loading, controller input, frame/audio streaming to PC
- **Standalone mode:** SD card ROM loading, VGA output, keyboard input, mono audio out

**Target compatibility:** Up to SMB3 size (256 KB PRG + 128 KB CHR = 384 KB)

---

## Clock Architecture

### Clock Domains

| Domain | Frequency     | Source       | Purpose                     |
| ------ | ------------- | ------------ | --------------------------- |
| NES    | 21.477272 MHz | MMCM #1      | CPU, PPU, APU, PWM, SPI     |
| VGA    | 40.000 MHz    | MMCM #2      | 800×600 @ 60 Hz pixel clock |
| RMII   | 50.000 MHz    | External PHY | Ethernet                    |

### MMCM Instantiation

Xilinx 7-series uses `MMCME2_ADV` primitive. Avoid clk_wiz IP — instantiate directly for a pure Hardcaml design.

```ocaml
(* In Hardcaml, instantiate as: *)
let mmcm =
  Instantiation.create ()
    ~name:"MMCME2_ADV"
    ~parameters:[
      Parameter.create ~name:"CLKFBOUT_MULT_F" ~value:(Float 42.955);
      Parameter.create ~name:"DIVCLK_DIVIDE" ~value:(Int 2);
      Parameter.create ~name:"CLKOUT0_DIVIDE_F" ~value:(Float 50.0);
      (* ... etc *)
    ]
    ~inputs:[
      "CLKIN1", clk_100mhz;
      "RST", gnd;
      "PWRDWN", gnd;
      "CLKFBIN", clkfb;
    ]
    ~outputs:[
      "CLKOUT0", Some 1;
      "CLKFBOUT", Some 1;
      "LOCKED", Some 1;
    ]
```

**To find correct parameters:** Use Vivado clk_wiz GUI once, note the calculated values, copy them into your Hardcaml instantiation.

**VCO constraints (Artix-7):** 600–1200 MHz

**Clock buffers:** MMCM outputs must go through `BUFG`:

```ocaml
let clk_nes =
  Instantiation.create ()
    ~name:"BUFG"
    ~inputs:["I", mmcm_clkout0]
    ~outputs:["O", Some 1]
```

### NES Internal Timing

Everything runs off the 21.477 MHz master clock. CPU and PPU have independent dividers to allow configurable phase alignment:

```
Master: 21.477 MHz
  │
  ├─÷4──► PPU:  5.369 MHz
  ├─÷12─► CPU:  1.789 MHz (phase offset: 0-3 master clocks)
  └─÷24─► APU:  894.9 kHz (every 2 CPU clocks)
```

```ocaml
(* Independent counters for CPU/PPU alignment control *)
let ppu_counter = reg spec ~width:2 in  (* 0-3 *)
let cpu_counter = reg spec ~width:4 in  (* 0-11 *)

let ppu_tick = ppu_counter ==:. 0 in
let cpu_tick = cpu_counter ==:. 0 in

(* Phase offset set at reset for different CPU/PPU alignments *)
let cpu_phase_offset = input "cpu_phase_offset" ~width:2 in

Always.(compile [
  when_ reset [
    ppu_counter <--. 0;
    cpu_counter <-- uresize cpu_phase_offset 4;
  ];
  ppu_counter <-- mux2 (ppu_counter ==:. 3) (zero 2) (ppu_counter +:. 1);
  cpu_counter <-- mux2 (cpu_counter ==:. 11) (zero 4) (cpu_counter +:. 1);
])
```

**Why master clock matters:** Real hardware has 4 possible CPU/PPU phase alignments depending on power-up timing. Some games have bugs that only manifest in certain alignments. Keeping the master clock lets you test all 4.

**Key insight:** Single clock domain, multiple enables. No CDC between NES components.

---

## BRAM Architecture

### Budget

**Available:** 4,860 Kbit = 607 KB

| Component         | Size    | Notes                  |
| ----------------- | ------- | ---------------------- |
| PRG ROM           | 256 KB  | SMB3 max               |
| CHR ROM           | 128 KB  | SMB3 max               |
| Frame buffer ×2   | 122 KB  | 256×240×1 byte × 2     |
| CPU RAM           | 2 KB    |                        |
| PRG RAM (WRAM)    | 8 KB    | Battery save           |
| VRAM (nametables) | 2 KB    |                        |
| Palette           | 32 B    |                        |
| OAM               | 256 B   |                        |
| Secondary OAM     | 32 B    |                        |
| Audio buffer      | 2 KB    | For Ethernet streaming |
| Packet buffer     | 2 KB    | Ethernet RX            |
| **Total**         | ~523 KB |                        |
| **Margin**        | ~84 KB  |                        |

### Hardcaml Xilinx BRAM

Use `hardcaml_xilinx` library for portable BRAM instantiation:

```ocaml
open Hardcaml_xilinx

(* True dual-port RAM - both ports can read/write *)
let tdp_ram =
  True_dual_port_ram.create
    ~memory_optimization:false
    ~cascade_height:Inferred
    ~arch:Ultraram  (* or Blockram or Distributed *)
    ~build_mode:Synthesis
    ()
    ~clock_a:clk_nes
    ~clock_b:clk_vga
    ~clear_a:gnd
    ~clear_b:gnd
    ~size:65536
    ~port_a:{ address = addr_a; data = data_a; enable = en_a; ... }
    ~port_b:{ address = addr_b; data = data_b; enable = en_b; ... }
```

For simple single-port or simple dual-port (one read, one write):

```ocaml
(* Simple dual-port - Port A writes, Port B reads *)
let sdp_ram =
  Simple_dual_port_ram.create
    ~read_latency:1
    ~arch:Blockram
    ~build_mode:Synthesis
    ()
    ~clock:clk
    ~clear:gnd
    ~size:1024
    ~write_address:wr_addr
    ~write_data:wr_data
    ~write_enable:wr_en
    ~read_address:rd_addr
    ~read_enable:rd_en
```

### Dual-Port for Clock Domain Crossing

BRAM is naturally dual-port with independent clocks per port:

```
Port A (NES 21 MHz)              Port B (VGA 40 MHz)
       │                                │
       ▼                                ▼
┌──────────────────────────────────────────┐
│              Block RAM                   │
│         (independent clock per port)     │
└──────────────────────────────────────────┘
```

**Frame buffer example:**
- Port A: NES domain writes pixels
- Port B: VGA domain reads pixels

No async FIFO needed for bulk data — BRAM handles it.

### Memory Map (suggested)

```
0x00000 - 0x3FFFF  PRG ROM (256 KB)
0x40000 - 0x5FFFF  CHR ROM (128 KB)
0x60000 - 0x6EFFF  Frame buffer 0 (61 KB)
0x6F000 - 0x7DFFF  Frame buffer 1 (61 KB)
0x7E000 - 0x7E7FF  CPU RAM (2 KB)
0x7E800 - 0x7FFFF  WRAM, VRAM, palette, OAM, buffers
```

---

## Clock Domain Crossing

### Where CDC Occurs

| From | To   | Data                 | Method                    |
| ---- | ---- | -------------------- | ------------------------- |
| NES  | VGA  | Frame pixels         | Dual-port BRAM            |
| NES  | RMII | Frame/audio data     | Dual-port BRAM            |
| RMII | NES  | ROM data, controller | Dual-port BRAM + sync FFs |
| NES  | VGA  | Frame bank pointer   | 2-FF synchronizer         |

### Synchronizers for Control Signals

For single-bit signals (flags, enables):

```ocaml
(* 2-FF synchronizer *)
let sync_ff1 = reg spec_dest async_signal in
let sync_ff2 = reg spec_dest sync_ff1 in
(* Use sync_ff2 in destination domain *)
```

For multi-bit values that change atomically (like a bank select):

```ocaml
(* Gray code not needed for single-bit bank pointer *)
let frame_bank_synced = 
  let ff1 = reg vga_spec frame_bank_nes in
  reg vga_spec ff1
```

### Frame Buffer Double-Buffering

```ocaml
(* NES domain *)
let frame_bank = reg nes_spec ~width:1 in
let frame_done = (* end of frame signal *) in
Always.(compile [
  when_ frame_done [
    frame_bank <-- ~:(frame_bank.value)
  ]
])

(* VGA domain - sync the bank pointer *)
let frame_bank_vga = sync_2ff vga_spec frame_bank in
let read_bank = ~:frame_bank_vga in  (* read opposite of write *)
```

---

## I/O Interfaces

### VGA (800×600 @ 60 Hz)

**Timing parameters:**

| Parameter     | Value  |
| ------------- | ------ |
| Pixel clock   | 40 MHz |
| H visible     | 800    |
| H front porch | 40     |
| H sync        | 128    |
| H back porch  | 88     |
| H total       | 1056   |
| V visible     | 600    |
| V front porch | 1      |
| V sync        | 4      |
| V back porch  | 23     |
| V total       | 628    |

**Signals (Nexys A7):**

```ocaml
module Vga_out = struct
  type 'a t = {
    vga_r : 'a [@bits 4];
    vga_g : 'a [@bits 4];
    vga_b : 'a [@bits 4];
    vga_hs : 'a;
    vga_vs : 'a;
  }
end
```

Nexys A7 has 12-bit VGA (4 bits per channel) via resistor DAC.

**Implementation:**

```ocaml
let h_count = reg spec ~width:11 in
let v_count = reg spec ~width:10 in

let h_sync = (h_count >=:. 840) &: (h_count <:. 968) in
let v_sync = (v_count >=:. 601) &: (v_count <:. 605) in
let visible = (h_count <:. 800) &: (v_count <:. 600) in

let h_next = mux2 (h_count ==:. 1055) (zero 11) (h_count +:. 1) in
let v_next = mux2 (h_count ==:. 1055)
  (mux2 (v_count ==:. 627) (zero 10) (v_count +:. 1))
  v_count
in

(* Scale 256×240 NES to 512×480 centered in 800×600 *)
let nes_x = (h_count -:. 144) >>:. 1 in  (* center offset, /2 scale *)
let nes_y = (v_count -:. 60) >>:. 1 in
let in_nes_area = (h_count >=:. 144) &: (h_count <:. 656) &:
                  (v_count >=:. 60) &: (v_count <:. 540) in
```

### Ethernet (RMII)

**LAN8720A PHY on Nexys A7:**

| Signal      | Direction | Width | Description                   |
| ----------- | --------- | ----- | ----------------------------- |
| eth_ref_clk | Out       | 1     | 50 MHz to PHY                 |
| eth_rstn    | Out       | 1     | PHY reset (active low)        |
| eth_tx_en   | Out       | 1     | TX enable                     |
| eth_txd     | Out       | 2     | TX data                       |
| eth_rx_dv   | In        | 1     | RX data valid                 |
| eth_rxd     | In        | 2     | RX data                       |
| eth_rxerr   | In        | 1     | RX error                      |
| eth_crs_dv  | In        | 1     | Carrier sense / RX data valid |
| eth_mdc     | Out       | 1     | Management clock              |
| eth_mdio    | Inout     | 1     | Management data               |

**RMII timing:**
- 50 MHz clock
- 2 bits per clock = 100 Mbps
- Data sampled on rising edge

**Receive state machine:**

```ocaml
type rx_state = Idle | Preamble | Data | Done

(* Shift in 2 bits per 50 MHz clock *)
let rx_byte = reg spec ~width:8 in
let rx_bit_count = reg spec ~width:2 in

Always.(compile [
  sm.switch [
    Idle, [
      when_ (eth_crs_dv &: (eth_rxd ==:. 0b01)) [
        sm.set_next Preamble
      ]
    ];
    Preamble, [
      when_ (eth_rxd ==:. 0b11) [  (* SFD *)
        sm.set_next Data;
        rx_bit_count <--. 0;
      ]
    ];
    Data, [
      rx_byte <-- eth_rxd @: rx_byte.:[7,2];
      rx_bit_count <-- rx_bit_count.value +:. 1;
      when_ (rx_bit_count.value ==:. 3) [
        (* Full byte received, write to buffer *)
      ];
      when_ ~:eth_crs_dv [
        sm.set_next Done
      ]
    ];
    (* ... *)
  ]
])
```

**Protocol stack (minimal):**

1. Ethernet frame: Check dest MAC
2. ARP: Reply to requests for your IP
3. IP: Parse header (optionally verify checksum)
4. UDP: Parse header, extract payload
5. Your protocol: ROM data, controller state, etc.

**IP checksum (ones' complement sum):**

```ocaml
(* Sum all 16-bit words, fold carry *)
let checksum_step sum word =
  let extended = uresize sum 17 +: uresize word 17 in
  extended.:[15,0] +: uresize extended.:[16,16] 17

let checksum = ~:(fold checksum_step (zero 16) header_words)
```

### Audio (PWM)

**Nexys A7 audio:**
- `aud_pwm`: PWM output
- `aud_sd`: Amplifier shutdown (active low, directly active high = enabled)

**Implementation:**

```ocaml
(* 8-bit PWM running at master clock *)
let pwm_counter = reg spec ~width:8 (pwm_counter +:. 1) in
let aud_pwm = apu_sample >: pwm_counter in
let aud_sd = vdd in  (* enable amplifier *)

(* PWM frequency: 21.477 MHz / 256 = 83.9 kHz *)
```

**Hardware-accurate path:** APU sample updates at 894 kHz. PWM comparator uses current value. Board RC filter smooths output.

**For Ethernet streaming:** Decimate 19:1 separately for UDP packets.

### SD Card (SPI mode)

**Run SD SPI from NES domain with clock enable:**

```ocaml
(* Divide down to ~400 kHz for init, ~12 MHz for data *)
let spi_clk_div = reg spec ~width:8 in
let spi_tick = spi_clk_div ==:. 0 in

let spi_clk_out = reg spec ~width:1 in
let spi_mosi = reg spec ~width:1 in
(* spi_miso is an input *)

Always.(compile [
  spi_clk_div <-- spi_clk_div.value +:. 1;
  when_ spi_tick [
    spi_clk_out <-- ~:(spi_clk_out.value);
    when_ spi_clk_out.value [
      (* Shift out MOSI on falling edge *)
      spi_mosi <-- shift_reg.:[7,7];
      shift_reg <-- shift_reg.:[6,0] @: spi_miso;
    ]
  ]
])
```

**SD init sequence:**
1. 74+ clock cycles with CS high
2. CMD0 (GO_IDLE) → R1 response
3. CMD8 (SEND_IF_COND) → Check voltage
4. ACMD41 loop until ready
5. CMD58 (READ_OCR) → Check capacity
6. CMD17 (READ_SINGLE_BLOCK) for data

SD latency is 100μs–1ms per command. Only use for bulk ROM loading at startup.

### PS/2 Keyboard

**If using a PMOD PS/2 adapter:**

| Signal   | Description                       |
| -------- | --------------------------------- |
| ps2_clk  | Clock (device-driven, ~10-16 kHz) |
| ps2_data | Serial data input                 |

**Protocol:** 11-bit frame (start, 8 data, parity, stop) clocked by device.

```ocaml
(* Sample on falling edge of ps2_clk *)
let ps2_clk_prev = reg spec ps2_clk in
let ps2_falling = ps2_clk_prev &: ~:ps2_clk in

let bit_count = reg spec ~width:4 in
let shift_reg = reg spec ~width:11 in

Always.(compile [
  when_ ps2_falling [
    shift_reg <-- ps2_data @: shift_reg.:[10,1];
    bit_count <-- bit_count.value +:. 1;
    when_ (bit_count.value ==:. 10) [
      (* Full frame: shift_reg.:[8,1] is the scan code *)
      bit_count <--. 0;
    ]
  ]
])
```

Map scan codes to NES controller bits.

---

## DSP Usage

### NTSC Filter (Optional)

If implementing Blargg-style NTSC filtering on VGA output:

**Artix-7 100T:** 240 DSP48E1 slices

**Filter requirements:**
- ~12-24 tap FIR per output pixel
- 40 MHz output rate
- Can run DSPs at 2× (80 MHz) for resource sharing

**Estimate:** 8-16 DSP slices at 80 MHz. Plenty of headroom.

### APU Mixer

NES APU mixing involves nonlinear DAC curves. Can implement as:
- Lookup table (BRAM)
- Or approximation with DSP multiply-accumulate

Lookup table is simpler and accurate.

---

## Timing Constraints (XDC)

```tcl
# Input clock
create_clock -period 10.0 -name clk_100mhz [get_ports clk_100mhz]

# RMII clock from PHY
# FPGA generates 50 MHz to PHY and uses it back:
create_clock -period 20.0 -name clk_rmii [get_ports eth_rx_clk]

# Async clock groups
set_clock_groups -asynchronous \
  -group [get_clocks clk_nes] \
  -group [get_clocks clk_vga] \
  -group [get_clocks clk_rmii]

# Generated clocks (usually auto-inferred from MMCM)
# create_generated_clock -name clk_nes -source [get_pins mmcm_nes/CLKIN1] [get_pins mmcm_nes/CLKOUT0]

# I/O constraints - use Digilent's master XDC for pin locations
# Key timing constraints for RMII:
set_input_delay -clock clk_rmii -max 2.0 [get_ports {eth_rxd[*] eth_crs_dv}]
set_input_delay -clock clk_rmii -min 0.0 [get_ports {eth_rxd[*] eth_crs_dv}]
set_output_delay -clock clk_rmii -max 2.0 [get_ports {eth_txd[*] eth_tx_en}]
set_output_delay -clock clk_rmii -min 0.0 [get_ports {eth_txd[*] eth_tx_en}]
```

---

## Build Flow

### Directory Structure

```
nes_fpga/
├── src/
│   ├── nes/
│   │   ├── cpu.ml
│   │   ├── ppu.ml
│   │   ├── apu.ml
│   │   └── mapper.ml
│   ├── io/
│   │   ├── vga.ml
│   │   ├── ethernet.ml
│   │   ├── audio.ml
│   │   └── sd.ml
│   ├── memory/
│   │   └── bram.ml
│   ├── clocking.ml
│   └── top.ml
├── constraints/
│   └── nexys_a7.xdc
├── sim/
│   └── test_*.ml
└── dune
```

### Synthesis (No IP Dependencies)

```tcl
# Create project
create_project nes_fpga ./build -part xc7a100tcsg324-1

# Add Hardcaml-generated Verilog
add_files ./generated/top.v

# Add constraints
add_files -fileset constrs_1 ./constraints/nexys_a7.xdc

# Synthesize
synth_design -top top

# Implement
opt_design
place_design
route_design

# Generate bitstream
write_bitstream -force ./build/nes_fpga.bit
```

---

## Summary
| Aspect       | Approach                                                            |
| ------------ | ------------------------------------------------------------------- |
| Clocking     | 2 MMCMs: NES (21.477 MHz), VGA (40 MHz). RMII (50 MHz) external.    |
| NES timing   | Master clock base, independent CPU/PPU dividers for phase alignment |
| Memory       | All BRAM. Dual-port for CDC. ~523 KB used of 607 KB.                |
| Frame buffer | Double-buffered, NES writes one, VGA reads other                    |
| VGA          | 800×600 @ 60 Hz, integer-scaled 512×480 centered                    |
| Ethernet     | Minimal UDP stack: ARP reply, IP, UDP. Static IP.                   |
| Audio        | PWM at 84 kHz from full-rate APU output                             |
| SD           | SPI mode, bulk load only                                            |
| CDC          | BRAM for data, 2-FF sync for control signals                        |
| IP cores     | None. Direct primitive instantiation.                               |