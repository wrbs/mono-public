open! Core
open Hardcaml
open Hardcaml_lws

module Make (Stream : Hardcaml_axi.Stream.S) = struct
  let () =
    (* Some of the logic below uses lor to assemble the word. So we can't work with
       arbitrarily big bit widths.
    *)
    assert (Stream.Source.port_widths.tdata <= Int.num_bits)
  ;;

  open Stream

  let bytes_per_word = Source.port_widths.tdata / 8

  let send_packet
    (h @ local)
    ~(source : Bits.t ref Source.t)
    ~(dest : Bits.t ref Dest.t Before_and_after_edge.t)
    (data : string)
    =
    let data_bits = Source.port_widths.tdata in
    let len = String.length data in
    let num_words = (len + bytes_per_word - 1) / bytes_per_word in
    for word_idx = 0 to num_words - 1 do
      (* Calculate how many bytes are valid in this word *)
      let bytes_remaining = len - (word_idx * bytes_per_word) in
      let valid_bytes = Int.min bytes_remaining bytes_per_word in
      (* Build the data word from bytes *)
      let word_data = ref 0 in
      for byte_idx = 0 to bytes_per_word - 1 do
        let char_idx = (word_idx * bytes_per_word) + byte_idx in
        let byte_val =
          if char_idx < len then Char.to_int (String.get data char_idx) else 0
        in
        word_data := !word_data lor (byte_val lsl (byte_idx * 8))
      done;
      (* Build tkeep mask - 1s for valid bytes only *)
      let tkeep_val = (1 lsl valid_bytes) - 1 in
      (* Set source signals *)
      source.tdata := Bits.of_unsigned_int ~width:data_bits !word_data;
      source.tlast := Bits.of_bool (word_idx = num_words - 1);
      source.tkeep := Bits.of_unsigned_int ~width:bytes_per_word tkeep_val;
      source.tstrb := Bits.of_unsigned_int ~width:bytes_per_word tkeep_val;
      source.tuser := Bits.zero Source.port_widths.tuser;
      (* Randomly deassert tvalid with 10% probability *)
      while Float.( < ) (Random.float 1.0) 0.1 do
        source.tvalid := Bits.gnd;
        Lws.step h
      done;
      source.tvalid := Bits.vdd;
      (* Wait for tready *)
      Lws.step h;
      while not (Bits.to_bool !(dest.before_edge.tready)) do
        Lws.step h
      done
    done;
    (* Deassert tvalid after sending *)
    source.tvalid := Bits.gnd
  ;;

  let recv_packet
    (h @ local)
    ~(source : Bits.t ref Source.t Before_and_after_edge.t)
    ~(dest : Bits.t ref Dest.t)
    =
    let received = Buffer.create 42 in
    let done_ = ref false in
    while not !done_ do
      (* Randomly deassert tready with 10% probability *)
      if Float.( < ) (Random.float 1.0) 0.1
      then (
        dest.tready := Bits.gnd;
        Lws.step h)
      else (
        dest.tready := Bits.vdd;
        Lws.step h;
        if Bits.to_bool !(source.before_edge.tvalid)
        then (
          (* Extract bytes from data word, respecting tkeep *)
          let word = Bits.to_unsigned_int !(source.before_edge.tdata) in
          let tkeep = Bits.to_unsigned_int !(source.before_edge.tkeep) in
          for byte_idx = 0 to bytes_per_word - 1 do
            (* Only extract bytes marked as valid in tkeep *)
            if (tkeep lsr byte_idx) land 1 = 1
            then (
              let byte_val = (word lsr (byte_idx * 8)) land 0xFF in
              Buffer.add_char received (Char.of_int_exn byte_val))
          done;
          if Bits.to_bool !(source.before_edge.tlast) then done_ := true))
    done;
    dest.tready := Bits.gnd;
    Buffer.contents received
  ;;
end
