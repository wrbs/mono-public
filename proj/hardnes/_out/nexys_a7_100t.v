module tracer (
    cpu$s,
    cpu$p,
    cpu$y,
    cpu$x,
    cpu$a,
    cpu$pc,
    clear,
    clock,
    ready,
    cpu$fetching,
    rx_start,
    cpu$illegal,
    cpu$mem$addr,
    cpu$mem$data,
    cpu$mem$write,
    cpu$state,
    cpu_enable,
    tx,
    tx_start
);

    input [7:0] cpu$s;
    input [7:0] cpu$p;
    input [7:0] cpu$y;
    input [7:0] cpu$x;
    input [7:0] cpu$a;
    input [15:0] cpu$pc;
    input clear;
    input clock;
    input ready;
    input cpu$fetching;
    input rx_start;
    input cpu$illegal;
    input [15:0] cpu$mem$addr;
    input [7:0] cpu$mem$data;
    input cpu$mem$write;
    input [5:0] cpu$state;
    output cpu_enable;
    output [7:0] tx;
    output tx_start;

    wire _48;
    reg _50;
    wire tx_start_0;
    wire [7:0] _246;
    wire [7:0] _245;
    wire [7:0] _244;
    wire [7:0] _243;
    wire [7:0] _242;
    wire [7:0] _240;
    wire [7:0] _238;
    wire [7:0] _235;
    wire [7:0] _229;
    wire [7:0] _226;
    wire [7:0] _223;
    wire [7:0] _222;
    wire [7:0] _221;
    wire [7:0] _219;
    wire [7:0] _217;
    wire [7:0] _216;
    wire [7:0] _215;
    wire [7:0] _214;
    wire [7:0] _213;
    wire [7:0] _212;
    wire [7:0] _211;
    wire [7:0] _210;
    wire [7:0] _209;
    wire [7:0] _208;
    wire [3:0] _206;
    wire [3:0] _102;
    wire _166;
    wire [3:0] _167;
    wire _133;
    wire [3:0] _134;
    wire _125;
    wire [3:0] _126;
    wire _117;
    wire [3:0] _118;
    wire [3:0] _106;
    wire [3:0] _107;
    reg [3:0] _104;
    wire [3:0] _95;
    wire _105;
    wire [3:0] _108;
    wire _92;
    wire _91;
    wire _90;
    wire _88;
    wire _89;
    wire _86;
    wire _87;
    wire _83;
    wire _84;
    wire _85;
    wire _80;
    wire _81;
    wire _82;
    wire _77;
    wire _76;
    wire _78;
    wire _75;
    wire _79;
    wire [7:0] _93;
    wire _94;
    wire [3:0] _109;
    wire [16:0] _59;
    wire _62;
    wire [15:0] _61;
    wire [16:0] _63;
    wire [1:0] _53;
    wire [16:0] _57;
    reg [16:0] _65;
    wire [16:0] _3;
    reg [16:0] _60;
    wire _71;
    wire [2:0] _70;
    wire [3:0] _72;
    wire [3:0] _69;
    reg [3:0] _111;
    wire [3:0] _4;
    reg [3:0] _67;
    wire _115;
    wire [2:0] _114;
    wire [3:0] _116;
    wire [3:0] _113;
    reg [3:0] _119;
    wire [3:0] _5;
    reg [3:0] _97;
    wire _123;
    wire [2:0] _122;
    wire [3:0] _124;
    wire [3:0] _121;
    reg [3:0] _127;
    wire [3:0] _6;
    reg [3:0] _99;
    wire _131;
    wire [2:0] _130;
    wire [3:0] _132;
    wire [3:0] _129;
    reg [3:0] _135;
    wire [3:0] _7;
    reg [3:0] _101;
    wire _164;
    wire [2:0] _163;
    wire [3:0] _165;
    wire [3:0] _162;
    wire [2:0] _157;
    wire [2:0] _73;
    wire [2:0] _137;
    wire [2:0] _138;
    reg [2:0] _139;
    wire [2:0] _8;
    reg [2:0] _74;
    wire _158;
    wire [1:0] _159;
    wire [1:0] _110;
    wire [4:0] _154;
    wire [4:0] _141;
    wire [4:0] _143;
    wire [4:0] _144;
    reg [4:0] _145;
    wire [4:0] _9;
    reg [4:0] _142;
    wire _155;
    wire [1:0] _156;
    wire [1:0] _64;
    wire _149;
    wire _150;
    reg _152;
    wire start_bcd;
    wire [1:0] _153;
    reg [1:0] _160;
    wire [1:0] _11;
    (* fsm_encoding="one_hot" *)
    reg [1:0] _52;
    reg [3:0] _168;
    wire [3:0] _12;
    reg [3:0] _103;
    wire [3:0] _199;
    wire [7:0] _14;
    wire [3:0] _198;
    wire [3:0] _194;
    wire [7:0] _16;
    wire [3:0] _193;
    wire [3:0] _189;
    wire [7:0] _18;
    wire [3:0] _188;
    wire [3:0] _184;
    wire [7:0] _20;
    wire [3:0] _183;
    wire [3:0] _179;
    wire [7:0] _22;
    wire [3:0] _178;
    wire [3:0] _174;
    wire [3:0] _173;
    wire [3:0] _172;
    wire [15:0] _24;
    wire [3:0] _171;
    reg [3:0] _207;
    reg [7:0] _224;
    reg [7:0] _247;
    wire _275;
    wire _276;
    wire _277;
    wire _274;
    wire [1:0] _269;
    wire [5:0] _251;
    wire [5:0] _169;
    wire [5:0] _253;
    wire [5:0] _254;
    wire [5:0] _255;
    wire [5:0] _256;
    wire [5:0] _257;
    wire [5:0] _249;
    wire [5:0] _250;
    reg [5:0] _258;
    wire [5:0] _26;
    reg [5:0] step;
    wire _252;
    wire [1:0] _270;
    wire [1:0] _271;
    wire gnd;
    wire _259;
    wire vdd;
    wire _260;
    reg _261;
    wire _27;
    reg sent;
    wire [1:0] _272;
    wire [14:0] _146;
    wire [15:0] _54;
    wire _29;
    wire _31;
    wire [15:0] _262;
    wire [15:0] _263;
    wire [15:0] _264;
    wire [15:0] _32;
    reg [15:0] cycles_;
    wire [14:0] _56;
    wire _147;
    wire _34;
    wire _148;
    wire [1:0] _267;
    wire _36;
    wire [1:0] _268;
    wire _38;
    wire [1:0] _265;
    reg [1:0] _273;
    wire [1:0] _39;
    (* fsm_encoding="one_hot" *)
    reg [1:0] state;
    reg _278;
    wire cpu_enable_0;
    assign _48 = sent ? gnd : vdd;
    always @* begin
        case (state)
        2'b10:
            _50 <= _48;
        default:
            _50 <= gnd;
        endcase
    end
    assign tx_start_0 = _50;
    assign _246 = 8'b00001010;
    assign _245 = 8'b00001101;
    assign _244 = 8'b00111010;
    assign _243 = 8'b01000011;
    assign _242 = 8'b01011001;
    assign _240 = 8'b00100000;
    assign _238 = 8'b01010011;
    assign _235 = 8'b01010000;
    assign _229 = 8'b01011000;
    assign _226 = 8'b01000001;
    assign _223 = 8'b01000110;
    assign _222 = 8'b01000101;
    assign _221 = 8'b01000100;
    assign _219 = 8'b01000010;
    assign _217 = 8'b00111001;
    assign _216 = 8'b00111000;
    assign _215 = 8'b00110111;
    assign _214 = 8'b00110110;
    assign _213 = 8'b00110101;
    assign _212 = 8'b00110100;
    assign _211 = 8'b00110011;
    assign _210 = 8'b00110010;
    assign _209 = 8'b00110001;
    assign _208 = 8'b00110000;
    assign _206 = 4'b1010;
    assign _102 = 4'b0000;
    assign _166 = _93[4:4];
    assign _167 = _166 ? _108 : _103;
    assign _133 = _93[3:3];
    assign _134 = _133 ? _108 : _101;
    assign _125 = _93[2:2];
    assign _126 = _125 ? _108 : _99;
    assign _117 = _93[1:1];
    assign _118 = _117 ? _108 : _97;
    assign _106 = 4'b0011;
    assign _107 = _104 + _106;
    always @* begin
        case (_74)
        0:
            _104 <= _67;
        1:
            _104 <= _97;
        2:
            _104 <= _99;
        3:
            _104 <= _101;
        default:
            _104 <= _103;
        endcase
    end
    assign _95 = 4'b0100;
    assign _105 = _95 < _104;
    assign _108 = _105 ? _107 : _104;
    assign _92 = _88 & _86;
    assign _91 = _88 & _84;
    assign _90 = _88 & _81;
    assign _88 = ~ _75;
    assign _89 = _88 & _78;
    assign _86 = _83 & _80;
    assign _87 = _75 & _86;
    assign _83 = ~ _76;
    assign _84 = _83 & _77;
    assign _85 = _75 & _84;
    assign _80 = ~ _77;
    assign _81 = _76 & _80;
    assign _82 = _75 & _81;
    assign _77 = _74[0:0];
    assign _76 = _74[1:1];
    assign _78 = _76 & _77;
    assign _75 = _74[2:2];
    assign _79 = _75 & _78;
    assign _93 = { _79,
                   _82,
                   _85,
                   _87,
                   _89,
                   _90,
                   _91,
                   _92 };
    assign _94 = _93[0:0];
    assign _109 = _94 ? _108 : _67;
    assign _59 = 17'b00000000000000000;
    assign _62 = 1'b0;
    assign _61 = _60[15:0];
    assign _63 = { _61,
                   _62 };
    assign _53 = 2'b00;
    assign _57 = { _53,
                   _56 };
    always @* begin
        case (_52)
        2'b00:
            _65 <= _57;
        2'b01:
            _65 <= _63;
        default:
            _65 <= _60;
        endcase
    end
    assign _3 = _65;
    always @(posedge _31) begin
        if (_29)
            _60 <= _59;
        else
            _60 <= _3;
    end
    assign _71 = _60[16:16];
    assign _70 = _67[2:0];
    assign _72 = { _70,
                   _71 };
    assign _69 = start_bcd ? _102 : _67;
    always @* begin
        case (_52)
        2'b00:
            _111 <= _69;
        2'b01:
            _111 <= _72;
        2'b10:
            _111 <= _109;
        default:
            _111 <= _67;
        endcase
    end
    assign _4 = _111;
    always @(posedge _31) begin
        if (_29)
            _67 <= _102;
        else
            _67 <= _4;
    end
    assign _115 = _67[3:3];
    assign _114 = _97[2:0];
    assign _116 = { _114,
                    _115 };
    assign _113 = start_bcd ? _102 : _97;
    always @* begin
        case (_52)
        2'b00:
            _119 <= _113;
        2'b01:
            _119 <= _116;
        2'b10:
            _119 <= _118;
        default:
            _119 <= _97;
        endcase
    end
    assign _5 = _119;
    always @(posedge _31) begin
        if (_29)
            _97 <= _102;
        else
            _97 <= _5;
    end
    assign _123 = _97[3:3];
    assign _122 = _99[2:0];
    assign _124 = { _122,
                    _123 };
    assign _121 = start_bcd ? _102 : _99;
    always @* begin
        case (_52)
        2'b00:
            _127 <= _121;
        2'b01:
            _127 <= _124;
        2'b10:
            _127 <= _126;
        default:
            _127 <= _99;
        endcase
    end
    assign _6 = _127;
    always @(posedge _31) begin
        if (_29)
            _99 <= _102;
        else
            _99 <= _6;
    end
    assign _131 = _99[3:3];
    assign _130 = _101[2:0];
    assign _132 = { _130,
                    _131 };
    assign _129 = start_bcd ? _102 : _101;
    always @* begin
        case (_52)
        2'b00:
            _135 <= _129;
        2'b01:
            _135 <= _132;
        2'b10:
            _135 <= _134;
        default:
            _135 <= _101;
        endcase
    end
    assign _7 = _135;
    always @(posedge _31) begin
        if (_29)
            _101 <= _102;
        else
            _101 <= _7;
    end
    assign _164 = _101[3:3];
    assign _163 = _103[2:0];
    assign _165 = { _163,
                    _164 };
    assign _162 = start_bcd ? _102 : _103;
    assign _157 = 3'b100;
    assign _73 = 3'b000;
    assign _137 = 3'b001;
    assign _138 = _74 + _137;
    always @* begin
        case (_52)
        2'b01:
            _139 <= _73;
        2'b10:
            _139 <= _138;
        default:
            _139 <= _74;
        endcase
    end
    assign _8 = _139;
    always @(posedge _31) begin
        if (_29)
            _74 <= _73;
        else
            _74 <= _8;
    end
    assign _158 = _74 == _157;
    assign _159 = _158 ? _64 : _52;
    assign _110 = 2'b10;
    assign _154 = 5'b10000;
    assign _141 = 5'b00000;
    assign _143 = 5'b00001;
    assign _144 = _142 + _143;
    always @* begin
        case (_52)
        2'b00:
            _145 <= _141;
        2'b01:
            _145 <= _144;
        default:
            _145 <= _142;
        endcase
    end
    assign _9 = _145;
    always @(posedge _31) begin
        if (_29)
            _142 <= _141;
        else
            _142 <= _9;
    end
    assign _155 = _142 == _154;
    assign _156 = _155 ? _53 : _110;
    assign _64 = 2'b01;
    assign _149 = _148 ? vdd : gnd;
    assign _150 = _36 ? _149 : gnd;
    always @* begin
        case (state)
        2'b01:
            _152 <= _150;
        default:
            _152 <= gnd;
        endcase
    end
    assign start_bcd = _152;
    assign _153 = start_bcd ? _64 : _52;
    always @* begin
        case (_52)
        2'b00:
            _160 <= _153;
        2'b01:
            _160 <= _156;
        2'b10:
            _160 <= _159;
        default:
            _160 <= _52;
        endcase
    end
    assign _11 = _160;
    always @(posedge _31) begin
        if (_29)
            _52 <= _53;
        else
            _52 <= _11;
    end
    always @* begin
        case (_52)
        2'b00:
            _168 <= _162;
        2'b01:
            _168 <= _165;
        2'b10:
            _168 <= _167;
        default:
            _168 <= _103;
        endcase
    end
    assign _12 = _168;
    always @(posedge _31) begin
        if (_29)
            _103 <= _102;
        else
            _103 <= _12;
    end
    assign _199 = _14[3:0];
    assign _14 = cpu$s;
    assign _198 = _14[7:4];
    assign _194 = _16[3:0];
    assign _16 = cpu$p;
    assign _193 = _16[7:4];
    assign _189 = _18[3:0];
    assign _18 = cpu$y;
    assign _188 = _18[7:4];
    assign _184 = _20[3:0];
    assign _20 = cpu$x;
    assign _183 = _20[7:4];
    assign _179 = _22[3:0];
    assign _22 = cpu$a;
    assign _178 = _22[7:4];
    assign _174 = _24[3:0];
    assign _173 = _24[7:4];
    assign _172 = _24[11:8];
    assign _24 = cpu$pc;
    assign _171 = _24[15:12];
    always @* begin
        case (step)
        0:
            _207 <= _171;
        1:
            _207 <= _172;
        2:
            _207 <= _173;
        3:
            _207 <= _174;
        4:
            _207 <= _206;
        5:
            _207 <= _206;
        6:
            _207 <= _206;
        7:
            _207 <= _178;
        8:
            _207 <= _179;
        9:
            _207 <= _206;
        10:
            _207 <= _206;
        11:
            _207 <= _206;
        12:
            _207 <= _183;
        13:
            _207 <= _184;
        14:
            _207 <= _206;
        15:
            _207 <= _206;
        16:
            _207 <= _206;
        17:
            _207 <= _188;
        18:
            _207 <= _189;
        19:
            _207 <= _206;
        20:
            _207 <= _206;
        21:
            _207 <= _206;
        22:
            _207 <= _193;
        23:
            _207 <= _194;
        24:
            _207 <= _206;
        25:
            _207 <= _206;
        26:
            _207 <= _206;
        27:
            _207 <= _198;
        28:
            _207 <= _199;
        29:
            _207 <= _206;
        30:
            _207 <= _206;
        31:
            _207 <= _206;
        32:
            _207 <= _206;
        33:
            _207 <= _206;
        34:
            _207 <= _103;
        35:
            _207 <= _101;
        36:
            _207 <= _99;
        37:
            _207 <= _97;
        38:
            _207 <= _67;
        39:
            _207 <= _206;
        default:
            _207 <= _206;
        endcase
    end
    always @* begin
        case (_207)
        0:
            _224 <= _208;
        1:
            _224 <= _209;
        2:
            _224 <= _210;
        3:
            _224 <= _211;
        4:
            _224 <= _212;
        5:
            _224 <= _213;
        6:
            _224 <= _214;
        7:
            _224 <= _215;
        8:
            _224 <= _216;
        9:
            _224 <= _217;
        10:
            _224 <= _226;
        11:
            _224 <= _219;
        12:
            _224 <= _243;
        13:
            _224 <= _221;
        14:
            _224 <= _222;
        default:
            _224 <= _223;
        endcase
    end
    always @* begin
        case (step)
        0:
            _247 <= _224;
        1:
            _247 <= _224;
        2:
            _247 <= _224;
        3:
            _247 <= _224;
        4:
            _247 <= _240;
        5:
            _247 <= _226;
        6:
            _247 <= _244;
        7:
            _247 <= _224;
        8:
            _247 <= _224;
        9:
            _247 <= _240;
        10:
            _247 <= _229;
        11:
            _247 <= _244;
        12:
            _247 <= _224;
        13:
            _247 <= _224;
        14:
            _247 <= _240;
        15:
            _247 <= _242;
        16:
            _247 <= _244;
        17:
            _247 <= _224;
        18:
            _247 <= _224;
        19:
            _247 <= _240;
        20:
            _247 <= _235;
        21:
            _247 <= _244;
        22:
            _247 <= _224;
        23:
            _247 <= _224;
        24:
            _247 <= _240;
        25:
            _247 <= _238;
        26:
            _247 <= _244;
        27:
            _247 <= _224;
        28:
            _247 <= _224;
        29:
            _247 <= _240;
        30:
            _247 <= _243;
        31:
            _247 <= _242;
        32:
            _247 <= _243;
        33:
            _247 <= _244;
        34:
            _247 <= _224;
        35:
            _247 <= _224;
        36:
            _247 <= _224;
        37:
            _247 <= _224;
        38:
            _247 <= _224;
        39:
            _247 <= _245;
        default:
            _247 <= _246;
        endcase
    end
    assign _275 = _252 ? vdd : gnd;
    assign _276 = _34 ? _275 : gnd;
    assign _277 = sent ? _276 : gnd;
    assign _274 = _36 ? gnd : vdd;
    assign _269 = 2'b11;
    assign _251 = 6'b101000;
    assign _169 = 6'b000000;
    assign _253 = 6'b000001;
    assign _254 = step + _253;
    assign _255 = _252 ? step : _254;
    assign _256 = _34 ? _255 : step;
    assign _257 = sent ? _256 : step;
    assign _249 = _148 ? _169 : step;
    assign _250 = _36 ? _249 : step;
    always @* begin
        case (state)
        2'b01:
            _258 <= _250;
        2'b10:
            _258 <= _257;
        default:
            _258 <= step;
        endcase
    end
    assign _26 = _258;
    always @(posedge _31) begin
        if (_29)
            step <= _169;
        else
            step <= _26;
    end
    assign _252 = step == _251;
    assign _270 = _252 ? _269 : state;
    assign _271 = _34 ? _270 : state;
    assign gnd = 1'b0;
    assign _259 = _34 ? gnd : sent;
    assign vdd = 1'b1;
    assign _260 = sent ? _259 : vdd;
    always @* begin
        case (state)
        2'b10:
            _261 <= _260;
        default:
            _261 <= sent;
        endcase
    end
    assign _27 = _261;
    always @(posedge _31) begin
        if (_29)
            sent <= _62;
        else
            sent <= _27;
    end
    assign _272 = sent ? _271 : state;
    assign _146 = 15'b110011110111010;
    assign _54 = 16'b0000000000000000;
    assign _29 = clear;
    assign _31 = clock;
    assign _262 = 16'b0000000000000001;
    assign _263 = cycles_ + _262;
    assign _264 = cpu_enable_0 ? _263 : cycles_;
    assign _32 = _264;
    always @(posedge _31) begin
        if (_29)
            cycles_ <= _54;
        else
            cycles_ <= _32;
    end
    assign _56 = cycles_[15:1];
    assign _147 = _56 < _146;
    assign _34 = ready;
    assign _148 = _34 & _147;
    assign _267 = _148 ? _110 : state;
    assign _36 = cpu$fetching;
    assign _268 = _36 ? _267 : state;
    assign _38 = rx_start;
    assign _265 = _38 ? _64 : state;
    always @* begin
        case (state)
        2'b00:
            _273 <= _265;
        2'b01:
            _273 <= _268;
        2'b10:
            _273 <= _272;
        2'b11:
            _273 <= _64;
        default:
            _273 <= state;
        endcase
    end
    assign _39 = _273;
    always @(posedge _31) begin
        if (_29)
            state <= _53;
        else
            state <= _39;
    end
    always @* begin
        case (state)
        2'b01:
            _278 <= _274;
        2'b10:
            _278 <= _277;
        2'b11:
            _278 <= vdd;
        default:
            _278 <= gnd;
        endcase
    end
    assign cpu_enable_0 = _278;
    assign cpu_enable = cpu_enable_0;
    assign tx = _247;
    assign tx_start = tx_start_0;

endmodule
module cpu (
    enable,
    clear,
    clock,
    data,
    irq,
    nmi,
    mem$addr,
    mem$data,
    mem$write,
    state,
    fetching,
    pc,
    a,
    s,
    x,
    y,
    p,
    illegal
);

    input enable;
    input clear;
    input clock;
    input [7:0] data;
    input irq;
    input nmi;
    output [15:0] mem$addr;
    output [7:0] mem$data;
    output mem$write;
    output [5:0] state;
    output fetching;
    output [15:0] pc;
    output [7:0] a;
    output [7:0] s;
    output [7:0] x;
    output [7:0] y;
    output [7:0] p;
    output illegal;

    wire _54;
    wire [1:0] _59;
    wire [7:0] _68;
    wire _80;
    wire _91;
    reg _92;
    wire mem_port$write;
    wire [7:0] _121;
    wire [7:0] _120;
    wire [7:0] _118;
    wire [7:0] _117;
    wire [7:0] _115;
    wire _102;
    wire _100;
    reg _98;
    reg _104;
    wire _12;
    reg flags$i;
    wire _109;
    wire _108;
    reg _107;
    reg _110;
    wire _13;
    reg flags$d;
    wire [7:0] _114;
    wire [7:0] _116;
    wire [7:0] _112;
    wire [7:0] _111;
    reg [7:0] _122;
    wire [7:0] mem_port$data;
    wire [15:0] _1929;
    wire [15:0] _1927;
    wire [15:0] _1926;
    wire [15:0] _1924;
    reg [15:0] _1928;
    wire [15:0] _1922;
    wire [15:0] _1921;
    wire [15:0] _1919;
    wire [1:0] _127;
    wire [1:0] _125;
    reg [1:0] _129;
    reg [1:0] _130;
    wire [1:0] _16;
    reg [1:0] interrupt_type$binary_variant = 2'b00;
    reg [15:0] _1923;
    wire [7:0] _1917;
    wire [15:0] stack_addr;
    wire [7:0] _1915;
    wire [15:0] _1916;
    wire [15:0] _1914;
    wire [15:0] _1912;
    wire [15:0] _1910;
    wire [15:0] _1908;
    wire [15:0] _1906;
    wire [7:0] _266;
    reg [7:0] _261;
    reg [7:0] _271;
    wire [7:0] _17;
    reg [7:0] addr_high;
    wire [15:0] effective_addr;
    wire [15:0] _1903;
    wire [5:0] _52;
    wire [5:0] _900;
    wire [5:0] _896;
    wire [5:0] _90;
    wire [5:0] _89;
    wire [5:0] _103;
    wire [5:0] _1140;
    wire [5:0] _893;
    wire [5:0] _355;
    wire [5:0] _101;
    wire [5:0] _891;
    wire [5:0] _888;
    wire [5:0] _354;
    wire [5:0] _886;
    wire [5:0] _85;
    wire [5:0] _84;
    wire [5:0] _882;
    wire [5:0] _353;
    wire [5:0] _1899;
    wire [7:0] _347;
    wire _340;
    wire [1:0] _341;
    wire [3:0] _342;
    wire [7:0] _343;
    wire [15:0] _344;
    wire [15:0] _898;
    wire [15:0] _899;
    wire [15:0] _895;
    wire [15:0] _892;
    wire [15:0] _889;
    wire [15:0] _890;
    wire [15:0] _887;
    wire [15:0] _885;
    wire [15:0] _881;
    wire [15:0] _880;
    wire [7:0] _350;
    wire [7:0] _351;
    wire [7:0] _338;
    wire [7:0] _335;
    wire [7:0] _332;
    wire [7:0] _327;
    wire [7:0] _320;
    wire [7:0] _313;
    wire [7:0] _306;
    wire [7:0] _299;
    wire [7:0] _292;
    wire [7:0] _284;
    wire [7:0] _277;
    reg [7:0] _331;
    reg [7:0] _356;
    wire [7:0] _18;
    reg [7:0] pointer;
    wire [15:0] _877;
    wire [15:0] _876;
    wire [15:0] _874;
    wire [15:0] _870;
    wire [15:0] _867;
    wire [15:0] _864;
    wire [15:0] _861;
    wire [15:0] _858;
    wire [15:0] _855;
    wire [15:0] _853;
    wire [15:0] _851;
    wire [15:0] _849;
    wire [15:0] _847;
    wire [15:0] _845;
    wire [15:0] _843;
    wire [15:0] _841;
    wire [15:0] _838;
    wire [15:0] _835;
    wire [15:0] _832;
    wire [15:0] _829;
    wire [15:0] _826;
    wire [15:0] _823;
    wire [15:0] _821;
    wire [15:0] _819;
    wire [15:0] _817;
    wire [15:0] _815;
    wire [15:0] _813;
    wire [15:0] _810;
    wire [15:0] _808;
    wire [15:0] _805;
    wire [15:0] _802;
    wire [15:0] _799;
    wire [15:0] _796;
    wire [15:0] _793;
    wire [15:0] _790;
    wire [15:0] _787;
    wire [15:0] _785;
    wire [15:0] _783;
    wire [15:0] _781;
    wire [15:0] _779;
    wire [15:0] _777;
    wire [15:0] _775;
    wire [15:0] _773;
    wire [15:0] _770;
    wire [15:0] _767;
    wire [15:0] _764;
    wire [15:0] _761;
    wire [15:0] _758;
    wire [15:0] _755;
    wire [15:0] _753;
    wire [15:0] _751;
    wire [15:0] _749;
    wire [15:0] _747;
    wire [15:0] _745;
    wire [15:0] _742;
    wire [15:0] _740;
    wire [15:0] _737;
    wire [15:0] _734;
    wire [15:0] _731;
    wire [15:0] _728;
    wire [15:0] _725;
    wire [15:0] _722;
    wire [15:0] _720;
    wire [15:0] _718;
    wire [15:0] _716;
    wire [15:0] _714;
    wire [15:0] _712;
    wire [15:0] _710;
    wire [15:0] _708;
    wire [15:0] _705;
    wire [15:0] _702;
    wire [15:0] _699;
    wire [15:0] _696;
    wire [15:0] _693;
    wire [15:0] _690;
    wire [15:0] _688;
    wire [15:0] _686;
    wire [15:0] _684;
    wire [15:0] _682;
    wire [15:0] _680;
    wire [15:0] _677;
    wire [15:0] _675;
    wire [15:0] _672;
    wire [15:0] _669;
    wire [15:0] _666;
    wire [15:0] _663;
    wire [15:0] _660;
    wire [15:0] _657;
    wire [15:0] _655;
    wire [15:0] _653;
    wire [15:0] _651;
    wire [15:0] _649;
    wire [15:0] _647;
    wire [15:0] _645;
    wire [15:0] _643;
    wire [15:0] _640;
    wire [15:0] _637;
    wire [15:0] _634;
    wire [15:0] _631;
    wire [15:0] _628;
    wire [15:0] _626;
    wire [15:0] _624;
    wire [15:0] _622;
    wire [15:0] _620;
    wire [15:0] _618;
    wire [15:0] _615;
    wire [15:0] _613;
    wire [15:0] _610;
    wire [15:0] _607;
    wire [15:0] _604;
    wire [15:0] _601;
    wire [15:0] _598;
    wire [15:0] _595;
    wire [15:0] _592;
    wire [15:0] _590;
    wire [15:0] _588;
    wire [15:0] _586;
    wire [15:0] _584;
    wire [15:0] _582;
    wire [15:0] _580;
    wire [15:0] _578;
    wire [15:0] _575;
    wire [15:0] _572;
    wire [15:0] _569;
    wire [15:0] _566;
    wire [15:0] _563;
    wire [15:0] _560;
    wire [15:0] _558;
    wire [15:0] _556;
    wire [15:0] _554;
    wire [15:0] _552;
    wire [15:0] _550;
    wire [15:0] _548;
    wire [15:0] _545;
    wire [15:0] _542;
    wire [15:0] _539;
    wire [15:0] _536;
    wire [15:0] _533;
    wire [15:0] _530;
    wire [15:0] _528;
    wire [15:0] _526;
    wire [15:0] _524;
    wire [15:0] _522;
    wire [15:0] _520;
    wire [15:0] _518;
    wire [15:0] _516;
    wire [15:0] _513;
    wire [15:0] _510;
    wire [15:0] _507;
    wire [15:0] _505;
    wire [15:0] _502;
    wire [15:0] _499;
    wire [15:0] _497;
    wire [15:0] _495;
    wire [15:0] _493;
    wire [15:0] _491;
    wire [15:0] _489;
    wire [15:0] _487;
    wire [15:0] _484;
    wire [15:0] _481;
    wire [15:0] _478;
    wire [15:0] _475;
    wire [15:0] _472;
    wire [15:0] _469;
    wire [15:0] _467;
    wire [15:0] _465;
    wire [15:0] _463;
    wire [15:0] _461;
    wire [15:0] _459;
    wire [15:0] _457;
    wire [15:0] _455;
    wire [15:0] _452;
    wire [15:0] _449;
    wire [15:0] _446;
    wire [15:0] _443;
    wire [15:0] _440;
    wire [15:0] _437;
    wire [15:0] _435;
    wire [15:0] _433;
    wire [15:0] _431;
    wire [15:0] _429;
    wire [15:0] _427;
    wire [15:0] _425;
    wire [15:0] _422;
    wire [15:0] _419;
    wire [15:0] _416;
    wire [15:0] _413;
    wire [15:0] _410;
    wire [15:0] _407;
    wire [15:0] _404;
    wire [15:0] _402;
    wire [15:0] _400;
    wire [15:0] _398;
    wire [15:0] _396;
    wire [15:0] _394;
    wire [15:0] _392;
    wire [15:0] _390;
    wire [15:0] _387;
    wire [15:0] _384;
    wire [15:0] _381;
    wire [15:0] _378;
    wire [15:0] _375;
    wire [15:0] _372;
    wire [15:0] _370;
    wire [15:0] _368;
    wire [15:0] _366;
    wire [15:0] _364;
    wire [15:0] _362;
    wire [15:0] _360;
    reg [15:0] _872;
    wire [15:0] _358;
    reg [15:0] _901;
    wire [15:0] _19;
    reg [15:0] pc_0;
    wire [15:0] _345;
    wire [7:0] _346;
    wire _348;
    wire _349;
    wire [5:0] _1900;
    wire [5:0] _1898;
    wire [5:0] _269;
    wire [5:0] _268;
    wire [5:0] _336;
    wire [5:0] _267;
    wire [5:0] _1897;
    wire [5:0] _83;
    wire [5:0] _1893;
    wire [5:0] _82;
    wire [5:0] _1762;
    wire [5:0] _1891;
    wire [5:0] _1892;
    wire rw;
    wire [5:0] _1894;
    wire [5:0] _1895;
    wire _326;
    wire [5:0] _1887;
    wire _1141;
    wire _1139;
    wire _1137;
    wire _1132;
    wire _1114;
    wire _1107;
    wire _1097;
    wire _1090;
    wire _1076;
    wire _1064;
    wire _1061;
    wire _1058;
    wire _1055;
    wire _1052;
    wire _1049;
    wire _1046;
    wire _1035;
    wire _1026;
    wire _1020;
    wire _1017;
    wire _1005;
    wire _1001;
    wire _998;
    wire _995;
    wire _992;
    wire _989;
    wire _985;
    wire _975;
    wire _965;
    wire _955;
    wire _952;
    wire _948;
    reg _1134;
    wire _1135;
    wire _935;
    wire _930;
    wire _925;
    wire _920;
    wire _917;
    wire _914;
    wire _911;
    wire _908;
    wire _905;
    reg _1136;
    reg _1142;
    wire _20;
    reg flags$z;
    wire _319;
    wire [5:0] _1883;
    wire _312;
    wire [5:0] _1880;
    wire _305;
    wire [5:0] _1877;
    wire _298;
    wire [5:0] _1873;
    wire [5:0] _270;
    wire [5:0] _1273;
    wire [5:0] _1257;
    wire _1184;
    wire _1183;
    wire _1177;
    wire _1176;
    wire _1178;
    wire _1173;
    wire _1172;
    wire _1174;
    wire _1175;
    wire _1179;
    wire _1169;
    wire _1168;
    wire _1170;
    wire _1165;
    wire _1164;
    wire _1166;
    wire _1167;
    wire _1171;
    wire _1162;
    wire _1161;
    wire _1163;
    wire _1158;
    wire _1157;
    wire _1159;
    wire _1154;
    wire _1153;
    wire _1155;
    wire _1156;
    wire _1160;
    wire _1152;
    wire _1149;
    wire _1148;
    wire _1150;
    wire _1145;
    wire _1144;
    wire _1146;
    wire _1147;
    wire _1151;
    reg _1180;
    wire _1181;
    reg _1182;
    reg _1185;
    wire _21;
    reg flags$v;
    wire _291;
    wire [5:0] _1867;
    wire [5:0] _878;
    wire [5:0] _86;
    wire [5:0] _1262;
    wire _283;
    wire [5:0] _1861;
    wire [5:0] _1276;
    wire [5:0] _1858;
    wire [5:0] _263;
    wire [5:0] _1294;
    wire [5:0] _339;
    wire [5:0] _352;
    wire _1849;
    wire _1848;
    wire _1847;
    wire _1843;
    wire _1842;
    wire _1841;
    wire _1840;
    wire _1839;
    wire [7:0] _1074;
    wire _1838;
    wire _1837;
    wire _1836;
    wire _1835;
    wire _1834;
    wire _1833;
    wire _1832;
    wire _1831;
    wire _1830;
    wire _1829;
    wire _1828;
    wire _1827;
    wire _1826;
    wire _1825;
    wire _1824;
    wire _1823;
    wire _1822;
    wire _1821;
    wire [7:0] _983;
    wire _1820;
    wire [7:0] _973;
    wire _1819;
    wire [7:0] _963;
    wire _1818;
    wire _1817;
    wire _1816;
    wire _1815;
    reg _1844;
    wire _1845;
    wire _1814;
    wire _1813;
    wire _1812;
    wire _1811;
    wire _1810;
    wire _1809;
    wire _1808;
    wire _1807;
    reg [7:0] _1802;
    wire [7:0] _1803;
    wire [7:0] _923;
    wire [7:0] _1130;
    wire [7:0] _1112;
    wire [7:0] _1105;
    wire [7:0] _1095;
    wire [7:0] _1088;
    wire [6:0] _1023;
    wire [7:0] _1024;
    wire [7:0] _1015;
    wire [7:0] _1003;
    wire [7:0] _987;
    wire [7:0] _946;
    reg [7:0] _1798;
    wire [7:0] _1799;
    wire [7:0] _1193;
    wire [7:0] _1189;
    wire [7:0] _1188;
    reg [7:0] _1190;
    wire [7:0] _1187;
    wire [7:0] _1191;
    reg [7:0] _1194;
    wire [7:0] _22;
    reg [7:0] h;
    wire [6:0] _1110;
    wire [7:0] _1111;
    wire [6:0] _1102;
    wire [7:0] _1103;
    wire [6:0] _1099;
    wire [7:0] _1101;
    wire [7:0] _1104;
    wire [6:0] _1092;
    wire [7:0] _1094;
    wire [7:0] _1791;
    wire [6:0] _1788;
    wire [7:0] _1789;
    wire [7:0] _1792;
    wire _1241;
    wire _1240;
    wire _1127;
    wire [8:0] _1126;
    wire [9:0] _1128;
    wire [7:0] _1121;
    wire [6:0] _1118;
    wire [7:0] _1119;
    wire [7:0] _1122;
    wire [8:0] _1123;
    wire [8:0] _1116;
    wire [8:0] _1124;
    wire [9:0] _1125;
    wire [9:0] _1129;
    wire [1:0] _1233;
    wire _1235;
    wire _1236;
    wire _1232;
    wire _1231;
    wire _1230;
    wire [9:0] _1086;
    wire [7:0] _1080;
    wire [7:0] _1081;
    wire [8:0] _1082;
    wire [8:0] _1078;
    wire [8:0] _1083;
    wire [9:0] _1084;
    wire [9:0] _1087;
    wire [1:0] _1226;
    wire _1228;
    wire _1229;
    wire [8:0] _1072;
    wire [8:0] _1070;
    wire [7:0] _1067;
    wire [7:0] _1068;
    wire [8:0] _1069;
    wire [8:0] _1071;
    wire [8:0] _1073;
    wire [8:0] _1223;
    wire _1224;
    wire _1225;
    wire _1222;
    wire _1221;
    wire _1220;
    wire _1219;
    wire [8:0] _1216;
    wire _1217;
    wire _1218;
    wire [7:0] _1032;
    wire [6:0] _1029;
    wire [7:0] _1030;
    wire [7:0] _1033;
    wire _1215;
    wire [7:0] and_;
    wire _1214;
    wire [9:0] _1013;
    wire [7:0] _1008;
    wire [8:0] _1009;
    wire [8:0] _1007;
    wire [8:0] _1010;
    wire [9:0] _1011;
    wire [9:0] _1014;
    wire [1:0] _1210;
    wire _1212;
    wire _1213;
    wire [8:0] _981;
    wire [7:0] _977;
    wire [8:0] _978;
    wire [8:0] _980;
    wire [8:0] _982;
    wire [8:0] _1207;
    wire _1208;
    wire _1209;
    wire [8:0] _971;
    wire [7:0] _967;
    wire [8:0] _968;
    wire [8:0] _970;
    wire [8:0] _972;
    wire [8:0] _1204;
    wire _1205;
    wire _1206;
    wire [8:0] _961;
    wire [7:0] _957;
    wire [8:0] _958;
    wire [8:0] _960;
    wire [8:0] _962;
    wire [8:0] _1201;
    wire _1202;
    wire _1203;
    wire [9:0] _944;
    wire [8:0] _940;
    wire [8:0] _939;
    wire [8:0] _941;
    wire [9:0] _942;
    wire [9:0] _945;
    wire [1:0] _1197;
    wire _1199;
    wire _1200;
    reg _1237;
    wire _1238;
    reg _1239;
    reg _1242;
    wire _23;
    reg flags$c;
    wire [7:0] _1785;
    wire [6:0] _1781;
    wire [7:0] _1783;
    wire [7:0] _1786;
    wire [6:0] _1779;
    wire [7:0] _1780;
    wire [7:0] _1777;
    wire [7:0] _1775;
    wire [6:0] _1771;
    wire [7:0] _1773;
    wire [7:0] _1769;
    reg [7:0] _1796;
    wire [7:0] _24;
    reg [7:0] _1765;
    wire w;
    wire _1759;
    wire _1754;
    wire _1755;
    wire _1756;
    wire _25;
    reg _1309;
    wire _1307;
    wire [8:0] _1296;
    wire [7:0] _1304;
    wire [8:0] _1300;
    wire [8:0] _1299;
    wire [8:0] _1301;
    wire [7:0] _1302;
    wire [7:0] _1298;
    wire [7:0] _1041;
    wire [8:0] _1042;
    wire [7:0] _1037;
    wire [8:0] _1038;
    wire [8:0] _1040;
    wire [8:0] _1043;
    wire [7:0] _1044;
    reg [7:0] _1247;
    reg [7:0] _1248;
    wire [7:0] operand;
    reg [7:0] _1284;
    wire [7:0] _1285;
    wire [7:0] _933;
    wire [7:0] _928;
    wire [7:0] _1282;
    wire [7:0] _1280;
    wire [7:0] _1278;
    wire [7:0] _1275;
    wire [7:0] _1272;
    wire [7:0] _1270;
    wire [7:0] _1268;
    wire [7:0] _1266;
    wire [7:0] _1264;
    wire [7:0] _1261;
    wire [7:0] _1259;
    wire [7:0] _1256;
    wire [7:0] _1254;
    wire [7:0] _1252;
    reg [7:0] _1250;
    reg [7:0] _1283;
    wire [7:0] _27;
    reg [7:0] regs$s;
    reg [7:0] _1286;
    reg [7:0] _1287;
    wire [7:0] _28;
    reg [7:0] regs$x;
    reg [7:0] _1290;
    reg [7:0] _1291;
    wire [7:0] _29;
    reg [7:0] index_reg;
    wire [7:0] _1293;
    reg [7:0] _1292;
    reg [7:0] _1305;
    wire [7:0] _30;
    reg [7:0] addr_low;
    wire [8:0] _1295;
    wire [8:0] _1297;
    wire _1306;
    reg _1310;
    wire index_carry;
    wire _1751;
    wire [2:0] _1748;
    wire [2:0] _1745;
    wire [2:0] _1729;
    wire [5:0] _1603;
    wire [5:0] _1547;
    wire [5:0] _1501;
    wire [5:0] _1405;
    wire [5:0] _1379;
    wire [5:0] _1355;
    reg [5:0] _1709;
    reg [5:0] _1710;
    wire [5:0] _32;
    reg [5:0] op$binary_variant;
    reg [2:0] _1749;
    wire r;
    wire _1752;
    wire _1753;
    wire gnd;
    reg _1711;
    reg _1757;
    wire trigger_execute;
    wire _1760;
    wire vdd;
    reg _1761;
    reg _1763;
    wire execute_unit_enable;
    wire [7:0] _1797;
    wire [7:0] execute_result;
    reg [7:0] _1800;
    reg [7:0] _1801;
    wire [7:0] _36;
    reg [7:0] regs$a;
    reg [7:0] _1804;
    reg [7:0] _1805;
    wire [7:0] _37;
    reg [7:0] regs$y;
    wire [7:0] _903;
    wire _1806;
    reg _1846;
    reg _1850;
    wire _38;
    reg flags$n;
    wire _274;
    wire [5:0] _1855;
    wire [5:0] _88;
    wire _40;
    wire _42;
    wire _44;
    wire [7:0] _46;
    reg [7:0] _1851;
    wire [7:0] _47;
    reg [7:0] instr;
    reg [5:0] _1896;
    wire [5:0] _1852;
    reg [5:0] _1901;
    wire [5:0] _48;
    (* fsm_encoding="one_hot" *)
    reg [5:0] state_0;
    reg [15:0] _1930;
    wire [15:0] mem_port$addr;
    assign _54 = _1501 == state_0;
    assign _59 = 2'b10;
    assign _68 = { flags$n,
                   flags$v,
                   _59,
                   flags$d,
                   flags$i,
                   flags$z,
                   flags$c };
    assign _80 = _1405 == state_0;
    assign _91 = 1'b0;
    always @* begin
        case (state_0)
        6'b000101:
            _92 <= vdd;
        6'b000111:
            _92 <= vdd;
        6'b011000:
            _92 <= vdd;
        6'b011001:
            _92 <= vdd;
        6'b100011:
            _92 <= vdd;
        6'b100100:
            _92 <= vdd;
        6'b101010:
            _92 <= vdd;
        6'b101011:
            _92 <= vdd;
        6'b101100:
            _92 <= vdd;
        default:
            _92 <= _91;
        endcase
    end
    assign mem_port$write = _92;
    assign _121 = 8'b00000000;
    assign _120 = { flags$n,
                    flags$v,
                    _59,
                    flags$d,
                    flags$i,
                    flags$z,
                    flags$c };
    assign _118 = pc_0[7:0];
    assign _117 = pc_0[15:8];
    assign _115 = 8'b00010000;
    assign _102 = _46[2:2];
    assign _100 = _46[2:2];
    always @* begin
        case (instr)
        8'b01011000:
            _98 <= gnd;
        8'b01111000:
            _98 <= vdd;
        default:
            _98 <= flags$i;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000011:
            _104 <= _98;
        6'b100000:
            _104 <= _100;
        6'b101000:
            _104 <= _102;
        6'b101100:
            _104 <= vdd;
        default:
            _104 <= flags$i;
        endcase
    end
    assign _12 = _104;
    always @(posedge _44) begin
        if (_42)
            flags$i <= _91;
        else
            if (_40)
                flags$i <= _12;
    end
    assign _109 = _46[3:3];
    assign _108 = _46[3:3];
    always @* begin
        case (instr)
        8'b11011000:
            _107 <= gnd;
        8'b11111000:
            _107 <= vdd;
        default:
            _107 <= flags$d;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000011:
            _110 <= _107;
        6'b100000:
            _110 <= _108;
        6'b101000:
            _110 <= _109;
        default:
            _110 <= flags$d;
        endcase
    end
    assign _13 = _110;
    always @(posedge _44) begin
        if (_42)
            flags$d <= _91;
        else
            if (_40)
                flags$d <= _13;
    end
    assign _114 = { flags$n,
                    flags$v,
                    _59,
                    flags$d,
                    flags$i,
                    flags$z,
                    flags$c };
    assign _116 = _114 | _115;
    assign _112 = pc_0[7:0];
    assign _111 = pc_0[15:8];
    always @* begin
        case (state_0)
        6'b000101:
            _122 <= execute_result;
        6'b000111:
            _122 <= _46;
        6'b011000:
            _122 <= _111;
        6'b011001:
            _122 <= _112;
        6'b100011:
            _122 <= regs$a;
        6'b100100:
            _122 <= _116;
        6'b101010:
            _122 <= _117;
        6'b101011:
            _122 <= _118;
        6'b101100:
            _122 <= _120;
        default:
            _122 <= _121;
        endcase
    end
    assign mem_port$data = _122;
    assign _1929 = 16'b0000000000000000;
    assign _1927 = 16'b1111111111111111;
    assign _1926 = 16'b1111111111111011;
    assign _1924 = 16'b1111111111111101;
    always @* begin
        case (interrupt_type$binary_variant)
        0:
            _1928 <= _1924;
        1:
            _1928 <= _1927;
        2:
            _1928 <= _1926;
        default:
            _1928 <= _1927;
        endcase
    end
    assign _1922 = 16'b1111111111111110;
    assign _1921 = 16'b1111111111111010;
    assign _1919 = 16'b1111111111111100;
    assign _127 = 2'b00;
    assign _125 = 2'b01;
    always @* begin
        case (instr)
        8'b00000000:
            _129 <= _125;
        default:
            _129 <= interrupt_type$binary_variant;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000000:
            _130 <= _127;
        6'b000011:
            _130 <= _129;
        default:
            _130 <= interrupt_type$binary_variant;
        endcase
    end
    assign _16 = _130;
    always @(posedge _44) begin
        if (_42)
            interrupt_type$binary_variant <= _127;
        else
            if (_40)
                interrupt_type$binary_variant <= _16;
    end
    always @* begin
        case (interrupt_type$binary_variant)
        0:
            _1923 <= _1919;
        1:
            _1923 <= _1922;
        2:
            _1923 <= _1921;
        default:
            _1923 <= _1922;
        endcase
    end
    assign _1917 = 8'b00000001;
    assign stack_addr = { _1917,
                          regs$s };
    assign _1915 = pc_0[7:0];
    assign _1916 = { pointer,
                     _1915 };
    assign _1914 = { _121,
                     pointer };
    assign _1912 = { _121,
                     pointer };
    assign _1910 = { _121,
                     pointer };
    assign _1908 = { _121,
                     pointer };
    assign _1906 = { _121,
                     pointer };
    assign _266 = index_carry ? h : addr_high;
    always @* begin
        case (instr)
        8'b00000100:
            _261 <= _121;
        8'b00000101:
            _261 <= _121;
        8'b00000110:
            _261 <= _121;
        8'b00000111:
            _261 <= _121;
        8'b00010100:
            _261 <= _121;
        8'b00010101:
            _261 <= _121;
        8'b00010110:
            _261 <= _121;
        8'b00010111:
            _261 <= _121;
        8'b00100100:
            _261 <= _121;
        8'b00100101:
            _261 <= _121;
        8'b00100110:
            _261 <= _121;
        8'b00100111:
            _261 <= _121;
        8'b00110100:
            _261 <= _121;
        8'b00110101:
            _261 <= _121;
        8'b00110110:
            _261 <= _121;
        8'b00110111:
            _261 <= _121;
        8'b01000100:
            _261 <= _121;
        8'b01000101:
            _261 <= _121;
        8'b01000110:
            _261 <= _121;
        8'b01000111:
            _261 <= _121;
        8'b01010100:
            _261 <= _121;
        8'b01010101:
            _261 <= _121;
        8'b01010110:
            _261 <= _121;
        8'b01010111:
            _261 <= _121;
        8'b01100100:
            _261 <= _121;
        8'b01100101:
            _261 <= _121;
        8'b01100110:
            _261 <= _121;
        8'b01100111:
            _261 <= _121;
        8'b01110100:
            _261 <= _121;
        8'b01110101:
            _261 <= _121;
        8'b01110110:
            _261 <= _121;
        8'b01110111:
            _261 <= _121;
        8'b10000100:
            _261 <= _121;
        8'b10000101:
            _261 <= _121;
        8'b10000110:
            _261 <= _121;
        8'b10000111:
            _261 <= _121;
        8'b10010100:
            _261 <= _121;
        8'b10010101:
            _261 <= _121;
        8'b10010110:
            _261 <= _121;
        8'b10010111:
            _261 <= _121;
        8'b10100100:
            _261 <= _121;
        8'b10100101:
            _261 <= _121;
        8'b10100110:
            _261 <= _121;
        8'b10100111:
            _261 <= _121;
        8'b10110100:
            _261 <= _121;
        8'b10110101:
            _261 <= _121;
        8'b10110110:
            _261 <= _121;
        8'b10110111:
            _261 <= _121;
        8'b11000100:
            _261 <= _121;
        8'b11000101:
            _261 <= _121;
        8'b11000110:
            _261 <= _121;
        8'b11000111:
            _261 <= _121;
        8'b11010100:
            _261 <= _121;
        8'b11010101:
            _261 <= _121;
        8'b11010110:
            _261 <= _121;
        8'b11010111:
            _261 <= _121;
        8'b11100100:
            _261 <= _121;
        8'b11100101:
            _261 <= _121;
        8'b11100110:
            _261 <= _121;
        8'b11100111:
            _261 <= _121;
        8'b11110100:
            _261 <= _121;
        8'b11110101:
            _261 <= _121;
        8'b11110110:
            _261 <= _121;
        8'b11110111:
            _261 <= _121;
        default:
            _261 <= addr_high;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000011:
            _271 <= _261;
        6'b001000:
            _271 <= _46;
        6'b001010:
            _271 <= _46;
        6'b001011:
            _271 <= _266;
        6'b001110:
            _271 <= _46;
        6'b010000:
            _271 <= _46;
        6'b010100:
            _271 <= _46;
        default:
            _271 <= addr_high;
        endcase
    end
    assign _17 = _271;
    always @(posedge _44) begin
        if (_42)
            addr_high <= _121;
        else
            if (_40)
                addr_high <= _17;
    end
    assign effective_addr = { addr_high,
                              addr_low };
    assign _1903 = { _121,
                     instr };
    assign _52 = 6'b000000;
    assign _900 = 6'b101110;
    assign _896 = 6'b101101;
    assign _90 = 6'b101100;
    assign _89 = 6'b101011;
    assign _103 = 6'b101000;
    assign _1140 = 6'b100110;
    assign _893 = 6'b100010;
    assign _355 = 6'b100001;
    assign _101 = 6'b100000;
    assign _891 = 6'b011110;
    assign _888 = 6'b011101;
    assign _354 = 6'b011100;
    assign _886 = 6'b011010;
    assign _85 = 6'b011001;
    assign _84 = 6'b011000;
    assign _882 = 6'b010110;
    assign _353 = 6'b010101;
    assign _1899 = 6'b010010;
    assign _347 = pc_0[15:8];
    assign _340 = pointer[7:7];
    assign _341 = { _340,
                    _340 };
    assign _342 = { _341,
                    _341 };
    assign _343 = { _342,
                    _342 };
    assign _344 = { _343,
                    pointer };
    assign _898 = { _46,
                    _121 };
    assign _899 = pc_0 | _898;
    assign _895 = { _121,
                    _46 };
    assign _892 = { _46,
                    pointer };
    assign _889 = 16'b0000000000000001;
    assign _890 = pc_0 + _889;
    assign _887 = { _46,
                    pointer };
    assign _885 = { _46,
                    addr_low };
    assign _881 = { _46,
                    pointer };
    assign _880 = pc_0 + _889;
    assign _350 = pc_0[15:8];
    assign _351 = _349 ? _350 : pointer;
    assign _338 = pointer + _1917;
    assign _335 = pointer + _1917;
    assign _332 = pointer + regs$x;
    assign _327 = _326 ? _46 : pointer;
    assign _320 = _319 ? _46 : pointer;
    assign _313 = _312 ? _46 : pointer;
    assign _306 = _305 ? _46 : pointer;
    assign _299 = _298 ? _46 : pointer;
    assign _292 = _291 ? _46 : pointer;
    assign _284 = _283 ? _46 : pointer;
    assign _277 = _274 ? _46 : pointer;
    always @* begin
        case (instr)
        8'b00000001:
            _331 <= _46;
        8'b00000011:
            _331 <= _46;
        8'b00010000:
            _331 <= _277;
        8'b00010001:
            _331 <= _46;
        8'b00010011:
            _331 <= _46;
        8'b00100001:
            _331 <= _46;
        8'b00100011:
            _331 <= _46;
        8'b00110000:
            _331 <= _284;
        8'b00110001:
            _331 <= _46;
        8'b00110011:
            _331 <= _46;
        8'b01000001:
            _331 <= _46;
        8'b01000011:
            _331 <= _46;
        8'b01001100:
            _331 <= _46;
        8'b01010000:
            _331 <= _292;
        8'b01010001:
            _331 <= _46;
        8'b01010011:
            _331 <= _46;
        8'b01100001:
            _331 <= _46;
        8'b01100011:
            _331 <= _46;
        8'b01110000:
            _331 <= _299;
        8'b01110001:
            _331 <= _46;
        8'b01110011:
            _331 <= _46;
        8'b10000001:
            _331 <= _46;
        8'b10000011:
            _331 <= _46;
        8'b10010000:
            _331 <= _306;
        8'b10010001:
            _331 <= _46;
        8'b10010011:
            _331 <= _46;
        8'b10100001:
            _331 <= _46;
        8'b10100011:
            _331 <= _46;
        8'b10110000:
            _331 <= _313;
        8'b10110001:
            _331 <= _46;
        8'b10110011:
            _331 <= _46;
        8'b11000001:
            _331 <= _46;
        8'b11000011:
            _331 <= _46;
        8'b11010000:
            _331 <= _320;
        8'b11010001:
            _331 <= _46;
        8'b11010011:
            _331 <= _46;
        8'b11100001:
            _331 <= _46;
        8'b11100011:
            _331 <= _46;
        8'b11110000:
            _331 <= _327;
        8'b11110001:
            _331 <= _46;
        8'b11110011:
            _331 <= _46;
        default:
            _331 <= pointer;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000011:
            _356 <= _331;
        6'b001100:
            _356 <= _332;
        6'b001101:
            _356 <= _335;
        6'b001111:
            _356 <= _338;
        6'b010001:
            _356 <= _351;
        6'b010101:
            _356 <= _46;
        6'b011100:
            _356 <= _46;
        6'b100001:
            _356 <= _46;
        default:
            _356 <= pointer;
        endcase
    end
    assign _18 = _356;
    always @(posedge _44) begin
        if (_42)
            pointer <= _121;
        else
            if (_40)
                pointer <= _18;
    end
    assign _877 = { _46,
                    pointer };
    assign _876 = pc_0 + _889;
    assign _874 = pc_0 + _889;
    assign _870 = pc_0 + _889;
    assign _867 = pc_0 + _889;
    assign _864 = pc_0 + _889;
    assign _861 = pc_0 + _889;
    assign _858 = pc_0 + _889;
    assign _855 = pc_0 + _889;
    assign _853 = pc_0 + _889;
    assign _851 = pc_0 + _889;
    assign _849 = pc_0 + _889;
    assign _847 = pc_0 + _889;
    assign _845 = pc_0 + _889;
    assign _843 = pc_0 + _889;
    assign _841 = pc_0 + _889;
    assign _838 = pc_0 + _889;
    assign _835 = pc_0 + _889;
    assign _832 = pc_0 + _889;
    assign _829 = pc_0 + _889;
    assign _826 = pc_0 + _889;
    assign _823 = pc_0 + _889;
    assign _821 = pc_0 + _889;
    assign _819 = pc_0 + _889;
    assign _817 = pc_0 + _889;
    assign _815 = pc_0 + _889;
    assign _813 = pc_0 + _889;
    assign _810 = pc_0 + _889;
    assign _808 = pc_0 + _889;
    assign _805 = pc_0 + _889;
    assign _802 = pc_0 + _889;
    assign _799 = pc_0 + _889;
    assign _796 = pc_0 + _889;
    assign _793 = pc_0 + _889;
    assign _790 = pc_0 + _889;
    assign _787 = pc_0 + _889;
    assign _785 = pc_0 + _889;
    assign _783 = pc_0 + _889;
    assign _781 = pc_0 + _889;
    assign _779 = pc_0 + _889;
    assign _777 = pc_0 + _889;
    assign _775 = pc_0 + _889;
    assign _773 = pc_0 + _889;
    assign _770 = pc_0 + _889;
    assign _767 = pc_0 + _889;
    assign _764 = pc_0 + _889;
    assign _761 = pc_0 + _889;
    assign _758 = pc_0 + _889;
    assign _755 = pc_0 + _889;
    assign _753 = pc_0 + _889;
    assign _751 = pc_0 + _889;
    assign _749 = pc_0 + _889;
    assign _747 = pc_0 + _889;
    assign _745 = pc_0 + _889;
    assign _742 = pc_0 + _889;
    assign _740 = pc_0 + _889;
    assign _737 = pc_0 + _889;
    assign _734 = pc_0 + _889;
    assign _731 = pc_0 + _889;
    assign _728 = pc_0 + _889;
    assign _725 = pc_0 + _889;
    assign _722 = pc_0 + _889;
    assign _720 = pc_0 + _889;
    assign _718 = pc_0 + _889;
    assign _716 = pc_0 + _889;
    assign _714 = pc_0 + _889;
    assign _712 = pc_0 + _889;
    assign _710 = pc_0 + _889;
    assign _708 = pc_0 + _889;
    assign _705 = pc_0 + _889;
    assign _702 = pc_0 + _889;
    assign _699 = pc_0 + _889;
    assign _696 = pc_0 + _889;
    assign _693 = pc_0 + _889;
    assign _690 = pc_0 + _889;
    assign _688 = pc_0 + _889;
    assign _686 = pc_0 + _889;
    assign _684 = pc_0 + _889;
    assign _682 = pc_0 + _889;
    assign _680 = pc_0 + _889;
    assign _677 = pc_0 + _889;
    assign _675 = pc_0 + _889;
    assign _672 = pc_0 + _889;
    assign _669 = pc_0 + _889;
    assign _666 = pc_0 + _889;
    assign _663 = pc_0 + _889;
    assign _660 = pc_0 + _889;
    assign _657 = pc_0 + _889;
    assign _655 = pc_0 + _889;
    assign _653 = pc_0 + _889;
    assign _651 = pc_0 + _889;
    assign _649 = pc_0 + _889;
    assign _647 = pc_0 + _889;
    assign _645 = pc_0 + _889;
    assign _643 = pc_0 + _889;
    assign _640 = pc_0 + _889;
    assign _637 = pc_0 + _889;
    assign _634 = pc_0 + _889;
    assign _631 = pc_0 + _889;
    assign _628 = pc_0 + _889;
    assign _626 = pc_0 + _889;
    assign _624 = pc_0 + _889;
    assign _622 = pc_0 + _889;
    assign _620 = pc_0 + _889;
    assign _618 = pc_0 + _889;
    assign _615 = pc_0 + _889;
    assign _613 = pc_0 + _889;
    assign _610 = pc_0 + _889;
    assign _607 = pc_0 + _889;
    assign _604 = pc_0 + _889;
    assign _601 = pc_0 + _889;
    assign _598 = pc_0 + _889;
    assign _595 = pc_0 + _889;
    assign _592 = pc_0 + _889;
    assign _590 = pc_0 + _889;
    assign _588 = pc_0 + _889;
    assign _586 = pc_0 + _889;
    assign _584 = pc_0 + _889;
    assign _582 = pc_0 + _889;
    assign _580 = pc_0 + _889;
    assign _578 = pc_0 + _889;
    assign _575 = pc_0 + _889;
    assign _572 = pc_0 + _889;
    assign _569 = pc_0 + _889;
    assign _566 = pc_0 + _889;
    assign _563 = pc_0 + _889;
    assign _560 = pc_0 + _889;
    assign _558 = pc_0 + _889;
    assign _556 = pc_0 + _889;
    assign _554 = pc_0 + _889;
    assign _552 = pc_0 + _889;
    assign _550 = pc_0 + _889;
    assign _548 = pc_0 + _889;
    assign _545 = pc_0 + _889;
    assign _542 = pc_0 + _889;
    assign _539 = pc_0 + _889;
    assign _536 = pc_0 + _889;
    assign _533 = pc_0 + _889;
    assign _530 = pc_0 + _889;
    assign _528 = pc_0 + _889;
    assign _526 = pc_0 + _889;
    assign _524 = pc_0 + _889;
    assign _522 = pc_0 + _889;
    assign _520 = pc_0 + _889;
    assign _518 = pc_0 + _889;
    assign _516 = pc_0 + _889;
    assign _513 = pc_0 + _889;
    assign _510 = pc_0 + _889;
    assign _507 = pc_0 + _889;
    assign _505 = pc_0 + _889;
    assign _502 = pc_0 + _889;
    assign _499 = pc_0 + _889;
    assign _497 = pc_0 + _889;
    assign _495 = pc_0 + _889;
    assign _493 = pc_0 + _889;
    assign _491 = pc_0 + _889;
    assign _489 = pc_0 + _889;
    assign _487 = pc_0 + _889;
    assign _484 = pc_0 + _889;
    assign _481 = pc_0 + _889;
    assign _478 = pc_0 + _889;
    assign _475 = pc_0 + _889;
    assign _472 = pc_0 + _889;
    assign _469 = pc_0 + _889;
    assign _467 = pc_0 + _889;
    assign _465 = pc_0 + _889;
    assign _463 = pc_0 + _889;
    assign _461 = pc_0 + _889;
    assign _459 = pc_0 + _889;
    assign _457 = pc_0 + _889;
    assign _455 = pc_0 + _889;
    assign _452 = pc_0 + _889;
    assign _449 = pc_0 + _889;
    assign _446 = pc_0 + _889;
    assign _443 = pc_0 + _889;
    assign _440 = pc_0 + _889;
    assign _437 = pc_0 + _889;
    assign _435 = pc_0 + _889;
    assign _433 = pc_0 + _889;
    assign _431 = pc_0 + _889;
    assign _429 = pc_0 + _889;
    assign _427 = pc_0 + _889;
    assign _425 = pc_0 + _889;
    assign _422 = pc_0 + _889;
    assign _419 = pc_0 + _889;
    assign _416 = pc_0 + _889;
    assign _413 = pc_0 + _889;
    assign _410 = pc_0 + _889;
    assign _407 = pc_0 + _889;
    assign _404 = pc_0 + _889;
    assign _402 = pc_0 + _889;
    assign _400 = pc_0 + _889;
    assign _398 = pc_0 + _889;
    assign _396 = pc_0 + _889;
    assign _394 = pc_0 + _889;
    assign _392 = pc_0 + _889;
    assign _390 = pc_0 + _889;
    assign _387 = pc_0 + _889;
    assign _384 = pc_0 + _889;
    assign _381 = pc_0 + _889;
    assign _378 = pc_0 + _889;
    assign _375 = pc_0 + _889;
    assign _372 = pc_0 + _889;
    assign _370 = pc_0 + _889;
    assign _368 = pc_0 + _889;
    assign _366 = pc_0 + _889;
    assign _364 = pc_0 + _889;
    assign _362 = pc_0 + _889;
    assign _360 = pc_0 + _889;
    always @* begin
        case (instr)
        8'b00000001:
            _872 <= _360;
        8'b00000011:
            _872 <= _362;
        8'b00000100:
            _872 <= _364;
        8'b00000101:
            _872 <= _366;
        8'b00000110:
            _872 <= _368;
        8'b00000111:
            _872 <= _370;
        8'b00001001:
            _872 <= _372;
        8'b00001011:
            _872 <= _375;
        8'b00001100:
            _872 <= _378;
        8'b00001101:
            _872 <= _381;
        8'b00001110:
            _872 <= _384;
        8'b00001111:
            _872 <= _387;
        8'b00010000:
            _872 <= _390;
        8'b00010001:
            _872 <= _392;
        8'b00010011:
            _872 <= _394;
        8'b00010100:
            _872 <= _396;
        8'b00010101:
            _872 <= _398;
        8'b00010110:
            _872 <= _400;
        8'b00010111:
            _872 <= _402;
        8'b00011001:
            _872 <= _404;
        8'b00011011:
            _872 <= _407;
        8'b00011100:
            _872 <= _410;
        8'b00011101:
            _872 <= _413;
        8'b00011110:
            _872 <= _416;
        8'b00011111:
            _872 <= _419;
        8'b00100000:
            _872 <= _422;
        8'b00100001:
            _872 <= _425;
        8'b00100011:
            _872 <= _427;
        8'b00100100:
            _872 <= _429;
        8'b00100101:
            _872 <= _431;
        8'b00100110:
            _872 <= _433;
        8'b00100111:
            _872 <= _435;
        8'b00101001:
            _872 <= _437;
        8'b00101011:
            _872 <= _440;
        8'b00101100:
            _872 <= _443;
        8'b00101101:
            _872 <= _446;
        8'b00101110:
            _872 <= _449;
        8'b00101111:
            _872 <= _452;
        8'b00110000:
            _872 <= _455;
        8'b00110001:
            _872 <= _457;
        8'b00110011:
            _872 <= _459;
        8'b00110100:
            _872 <= _461;
        8'b00110101:
            _872 <= _463;
        8'b00110110:
            _872 <= _465;
        8'b00110111:
            _872 <= _467;
        8'b00111001:
            _872 <= _469;
        8'b00111011:
            _872 <= _472;
        8'b00111100:
            _872 <= _475;
        8'b00111101:
            _872 <= _478;
        8'b00111110:
            _872 <= _481;
        8'b00111111:
            _872 <= _484;
        8'b01000001:
            _872 <= _487;
        8'b01000011:
            _872 <= _489;
        8'b01000100:
            _872 <= _491;
        8'b01000101:
            _872 <= _493;
        8'b01000110:
            _872 <= _495;
        8'b01000111:
            _872 <= _497;
        8'b01001001:
            _872 <= _499;
        8'b01001011:
            _872 <= _502;
        8'b01001100:
            _872 <= _505;
        8'b01001101:
            _872 <= _507;
        8'b01001110:
            _872 <= _510;
        8'b01001111:
            _872 <= _513;
        8'b01010000:
            _872 <= _516;
        8'b01010001:
            _872 <= _518;
        8'b01010011:
            _872 <= _520;
        8'b01010100:
            _872 <= _522;
        8'b01010101:
            _872 <= _524;
        8'b01010110:
            _872 <= _526;
        8'b01010111:
            _872 <= _528;
        8'b01011001:
            _872 <= _530;
        8'b01011011:
            _872 <= _533;
        8'b01011100:
            _872 <= _536;
        8'b01011101:
            _872 <= _539;
        8'b01011110:
            _872 <= _542;
        8'b01011111:
            _872 <= _545;
        8'b01100001:
            _872 <= _548;
        8'b01100011:
            _872 <= _550;
        8'b01100100:
            _872 <= _552;
        8'b01100101:
            _872 <= _554;
        8'b01100110:
            _872 <= _556;
        8'b01100111:
            _872 <= _558;
        8'b01101001:
            _872 <= _560;
        8'b01101011:
            _872 <= _563;
        8'b01101100:
            _872 <= _566;
        8'b01101101:
            _872 <= _569;
        8'b01101110:
            _872 <= _572;
        8'b01101111:
            _872 <= _575;
        8'b01110000:
            _872 <= _578;
        8'b01110001:
            _872 <= _580;
        8'b01110011:
            _872 <= _582;
        8'b01110100:
            _872 <= _584;
        8'b01110101:
            _872 <= _586;
        8'b01110110:
            _872 <= _588;
        8'b01110111:
            _872 <= _590;
        8'b01111001:
            _872 <= _592;
        8'b01111011:
            _872 <= _595;
        8'b01111100:
            _872 <= _598;
        8'b01111101:
            _872 <= _601;
        8'b01111110:
            _872 <= _604;
        8'b01111111:
            _872 <= _607;
        8'b10000000:
            _872 <= _610;
        8'b10000001:
            _872 <= _613;
        8'b10000010:
            _872 <= _615;
        8'b10000011:
            _872 <= _618;
        8'b10000100:
            _872 <= _620;
        8'b10000101:
            _872 <= _622;
        8'b10000110:
            _872 <= _624;
        8'b10000111:
            _872 <= _626;
        8'b10001001:
            _872 <= _628;
        8'b10001100:
            _872 <= _631;
        8'b10001101:
            _872 <= _634;
        8'b10001110:
            _872 <= _637;
        8'b10001111:
            _872 <= _640;
        8'b10010000:
            _872 <= _643;
        8'b10010001:
            _872 <= _645;
        8'b10010011:
            _872 <= _647;
        8'b10010100:
            _872 <= _649;
        8'b10010101:
            _872 <= _651;
        8'b10010110:
            _872 <= _653;
        8'b10010111:
            _872 <= _655;
        8'b10011001:
            _872 <= _657;
        8'b10011100:
            _872 <= _660;
        8'b10011101:
            _872 <= _663;
        8'b10011110:
            _872 <= _666;
        8'b10011111:
            _872 <= _669;
        8'b10100000:
            _872 <= _672;
        8'b10100001:
            _872 <= _675;
        8'b10100010:
            _872 <= _677;
        8'b10100011:
            _872 <= _680;
        8'b10100100:
            _872 <= _682;
        8'b10100101:
            _872 <= _684;
        8'b10100110:
            _872 <= _686;
        8'b10100111:
            _872 <= _688;
        8'b10101001:
            _872 <= _690;
        8'b10101011:
            _872 <= _693;
        8'b10101100:
            _872 <= _696;
        8'b10101101:
            _872 <= _699;
        8'b10101110:
            _872 <= _702;
        8'b10101111:
            _872 <= _705;
        8'b10110000:
            _872 <= _708;
        8'b10110001:
            _872 <= _710;
        8'b10110011:
            _872 <= _712;
        8'b10110100:
            _872 <= _714;
        8'b10110101:
            _872 <= _716;
        8'b10110110:
            _872 <= _718;
        8'b10110111:
            _872 <= _720;
        8'b10111001:
            _872 <= _722;
        8'b10111100:
            _872 <= _725;
        8'b10111101:
            _872 <= _728;
        8'b10111110:
            _872 <= _731;
        8'b10111111:
            _872 <= _734;
        8'b11000000:
            _872 <= _737;
        8'b11000001:
            _872 <= _740;
        8'b11000010:
            _872 <= _742;
        8'b11000011:
            _872 <= _745;
        8'b11000100:
            _872 <= _747;
        8'b11000101:
            _872 <= _749;
        8'b11000110:
            _872 <= _751;
        8'b11000111:
            _872 <= _753;
        8'b11001001:
            _872 <= _755;
        8'b11001011:
            _872 <= _758;
        8'b11001100:
            _872 <= _761;
        8'b11001101:
            _872 <= _764;
        8'b11001110:
            _872 <= _767;
        8'b11001111:
            _872 <= _770;
        8'b11010000:
            _872 <= _773;
        8'b11010001:
            _872 <= _775;
        8'b11010011:
            _872 <= _777;
        8'b11010100:
            _872 <= _779;
        8'b11010101:
            _872 <= _781;
        8'b11010110:
            _872 <= _783;
        8'b11010111:
            _872 <= _785;
        8'b11011001:
            _872 <= _787;
        8'b11011011:
            _872 <= _790;
        8'b11011100:
            _872 <= _793;
        8'b11011101:
            _872 <= _796;
        8'b11011110:
            _872 <= _799;
        8'b11011111:
            _872 <= _802;
        8'b11100000:
            _872 <= _805;
        8'b11100001:
            _872 <= _808;
        8'b11100010:
            _872 <= _810;
        8'b11100011:
            _872 <= _813;
        8'b11100100:
            _872 <= _815;
        8'b11100101:
            _872 <= _817;
        8'b11100110:
            _872 <= _819;
        8'b11100111:
            _872 <= _821;
        8'b11101001:
            _872 <= _823;
        8'b11101011:
            _872 <= _826;
        8'b11101100:
            _872 <= _829;
        8'b11101101:
            _872 <= _832;
        8'b11101110:
            _872 <= _835;
        8'b11101111:
            _872 <= _838;
        8'b11110000:
            _872 <= _841;
        8'b11110001:
            _872 <= _843;
        8'b11110011:
            _872 <= _845;
        8'b11110100:
            _872 <= _847;
        8'b11110101:
            _872 <= _849;
        8'b11110110:
            _872 <= _851;
        8'b11110111:
            _872 <= _853;
        8'b11111001:
            _872 <= _855;
        8'b11111011:
            _872 <= _858;
        8'b11111100:
            _872 <= _861;
        8'b11111101:
            _872 <= _864;
        8'b11111110:
            _872 <= _867;
        8'b11111111:
            _872 <= _870;
        default:
            _872 <= pc_0;
        endcase
    end
    assign _358 = pc_0 + _889;
    always @* begin
        case (state_0)
        6'b000010:
            _901 <= _358;
        6'b000011:
            _901 <= _872;
        6'b001000:
            _901 <= _874;
        6'b001010:
            _901 <= _876;
        6'b010001:
            _901 <= _345;
        6'b010011:
            _901 <= _877;
        6'b010100:
            _901 <= _880;
        6'b010110:
            _901 <= _881;
        6'b011010:
            _901 <= _885;
        6'b011101:
            _901 <= _887;
        6'b011110:
            _901 <= _890;
        6'b100010:
            _901 <= _892;
        6'b101101:
            _901 <= _895;
        6'b101110:
            _901 <= _899;
        default:
            _901 <= pc_0;
        endcase
    end
    assign _19 = _901;
    always @(posedge _44) begin
        if (_42)
            pc_0 <= _1929;
        else
            if (_40)
                pc_0 <= _19;
    end
    assign _345 = pc_0 + _344;
    assign _346 = _345[15:8];
    assign _348 = _346 == _347;
    assign _349 = ~ _348;
    assign _1900 = _349 ? _1899 : _1405;
    assign _1898 = _1755 ? _1895 : _267;
    assign _269 = 6'b010000;
    assign _268 = 6'b001110;
    assign _336 = 6'b001101;
    assign _267 = 6'b001011;
    assign _1897 = _1752 ? _1895 : _267;
    assign _83 = 6'b000111;
    assign _1893 = 6'b000110;
    assign _82 = 6'b000101;
    assign _1762 = 6'b000100;
    assign _1891 = r ? _1762 : state_0;
    assign _1892 = w ? _82 : _1891;
    assign rw = _1749[0:0];
    assign _1894 = rw ? _1893 : _1892;
    assign _1895 = trigger_execute ? _1894 : state_0;
    assign _326 = flags$z == vdd;
    assign _1887 = _326 ? _352 : _1405;
    assign _1141 = _46[1:1];
    assign _1139 = _46 == _121;
    assign _1137 = _46[1:1];
    assign _1132 = _1130 == _121;
    assign _1114 = _1112 == _121;
    assign _1107 = _1105 == _121;
    assign _1097 = _1095 == _121;
    assign _1090 = _1088 == _121;
    assign _1076 = _1074 == _121;
    assign _1064 = execute_result == _121;
    assign _1061 = execute_result == _121;
    assign _1058 = execute_result == _121;
    assign _1055 = execute_result == _121;
    assign _1052 = execute_result == _121;
    assign _1049 = execute_result == _121;
    assign _1046 = _1044 == _121;
    assign _1035 = _1033 == _121;
    assign _1026 = _1024 == _121;
    assign _1020 = and_ == _121;
    assign _1017 = _1015 == _121;
    assign _1005 = _1003 == _121;
    assign _1001 = operand == _121;
    assign _998 = operand == _121;
    assign _995 = operand == _121;
    assign _992 = operand == _121;
    assign _989 = _987 == _121;
    assign _985 = _983 == _121;
    assign _975 = _973 == _121;
    assign _965 = _963 == _121;
    assign _955 = and_ == _121;
    assign _952 = and_ == _121;
    assign _948 = _946 == _121;
    always @* begin
        case (op$binary_variant)
        6'b000001:
            _1134 <= _948;
        6'b000010:
            _1134 <= _952;
        6'b000011:
            _1134 <= _955;
        6'b000100:
            _1134 <= _965;
        6'b000101:
            _1134 <= _975;
        6'b000110:
            _1134 <= _985;
        6'b000111:
            _1134 <= _989;
        6'b001000:
            _1134 <= _992;
        6'b001001:
            _1134 <= _995;
        6'b001010:
            _1134 <= _998;
        6'b001011:
            _1134 <= _1001;
        6'b001100:
            _1134 <= _1005;
        6'b001101:
            _1134 <= _1017;
        6'b001110:
            _1134 <= _1020;
        6'b001111:
            _1134 <= _1026;
        6'b010000:
            _1134 <= _1035;
        6'b010001:
            _1134 <= _1046;
        6'b010110:
            _1134 <= _1049;
        6'b010111:
            _1134 <= _1052;
        6'b011000:
            _1134 <= _1055;
        6'b011001:
            _1134 <= _1058;
        6'b011010:
            _1134 <= _1061;
        6'b011011:
            _1134 <= _1064;
        6'b011100:
            _1134 <= _1076;
        6'b011101:
            _1134 <= _1090;
        6'b011110:
            _1134 <= _1097;
        6'b011111:
            _1134 <= _1107;
        6'b100000:
            _1134 <= _1114;
        6'b100001:
            _1134 <= _1132;
        default:
            _1134 <= flags$z;
        endcase
    end
    assign _1135 = execute_unit_enable ? _1134 : flags$z;
    assign _935 = _933 == _121;
    assign _930 = _928 == _121;
    assign _925 = _923 == _121;
    assign _920 = regs$s == _121;
    assign _917 = regs$a == _121;
    assign _914 = regs$a == _121;
    assign _911 = regs$y == _121;
    assign _908 = regs$x == _121;
    assign _905 = _903 == _121;
    always @* begin
        case (instr)
        8'b10001000:
            _1136 <= _905;
        8'b10001010:
            _1136 <= _908;
        8'b10011000:
            _1136 <= _911;
        8'b10101000:
            _1136 <= _914;
        8'b10101010:
            _1136 <= _917;
        8'b10111010:
            _1136 <= _920;
        8'b11001000:
            _1136 <= _925;
        8'b11001010:
            _1136 <= _930;
        8'b11101000:
            _1136 <= _935;
        default:
            _1136 <= _1135;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000011:
            _1142 <= _1136;
        6'b100000:
            _1142 <= _1137;
        6'b100110:
            _1142 <= _1139;
        6'b101000:
            _1142 <= _1141;
        default:
            _1142 <= _1135;
        endcase
    end
    assign _20 = _1142;
    always @(posedge _44) begin
        if (_42)
            flags$z <= _91;
        else
            if (_40)
                flags$z <= _20;
    end
    assign _319 = flags$z == gnd;
    assign _1883 = _319 ? _352 : _1405;
    assign _312 = flags$c == vdd;
    assign _1880 = _312 ? _352 : _1405;
    assign _305 = flags$c == gnd;
    assign _1877 = _305 ? _352 : _1405;
    assign _298 = flags$v == vdd;
    assign _1873 = _298 ? _352 : _1405;
    assign _270 = 6'b010100;
    assign _1273 = 6'b100101;
    assign _1257 = 6'b011011;
    assign _1184 = _46[6:6];
    assign _1183 = _46[6:6];
    assign _1177 = _1130[7:7];
    assign _1176 = regs$a[7:7];
    assign _1178 = _1176 ^ _1177;
    assign _1173 = _1122[7:7];
    assign _1172 = regs$a[7:7];
    assign _1174 = _1172 ^ _1173;
    assign _1175 = ~ _1174;
    assign _1179 = _1175 & _1178;
    assign _1169 = _1088[7:7];
    assign _1168 = regs$a[7:7];
    assign _1170 = _1168 ^ _1169;
    assign _1165 = _1081[7:7];
    assign _1164 = regs$a[7:7];
    assign _1166 = _1164 ^ _1165;
    assign _1167 = ~ _1166;
    assign _1171 = _1167 & _1170;
    assign _1162 = _1033[5:5];
    assign _1161 = _1033[6:6];
    assign _1163 = _1161 ^ _1162;
    assign _1158 = _1015[7:7];
    assign _1157 = regs$a[7:7];
    assign _1159 = _1157 ^ _1158;
    assign _1154 = _1008[7:7];
    assign _1153 = regs$a[7:7];
    assign _1155 = _1153 ^ _1154;
    assign _1156 = ~ _1155;
    assign _1160 = _1156 & _1159;
    assign _1152 = operand[6:6];
    assign _1149 = _946[7:7];
    assign _1148 = regs$a[7:7];
    assign _1150 = _1148 ^ _1149;
    assign _1145 = operand[7:7];
    assign _1144 = regs$a[7:7];
    assign _1146 = _1144 ^ _1145;
    assign _1147 = ~ _1146;
    assign _1151 = _1147 & _1150;
    always @* begin
        case (op$binary_variant)
        6'b000001:
            _1180 <= _1151;
        6'b000011:
            _1180 <= _1152;
        6'b001101:
            _1180 <= _1160;
        6'b010000:
            _1180 <= _1163;
        6'b011101:
            _1180 <= _1171;
        6'b100001:
            _1180 <= _1179;
        default:
            _1180 <= flags$v;
        endcase
    end
    assign _1181 = execute_unit_enable ? _1180 : flags$v;
    always @* begin
        case (instr)
        8'b10111000:
            _1182 <= gnd;
        default:
            _1182 <= _1181;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000011:
            _1185 <= _1182;
        6'b100000:
            _1185 <= _1183;
        6'b101000:
            _1185 <= _1184;
        default:
            _1185 <= _1181;
        endcase
    end
    assign _21 = _1185;
    always @(posedge _44) begin
        if (_42)
            flags$v <= _91;
        else
            if (_40)
                flags$v <= _21;
    end
    assign _291 = flags$v == gnd;
    assign _1867 = _291 ? _352 : _1405;
    assign _878 = 6'b010011;
    assign _86 = 6'b100011;
    assign _1262 = 6'b011111;
    assign _283 = flags$n == vdd;
    assign _1861 = _283 ? _352 : _1405;
    assign _1276 = 6'b100111;
    assign _1858 = 6'b010111;
    assign _263 = 6'b001010;
    assign _1294 = 6'b001001;
    assign _339 = 6'b001111;
    assign _352 = 6'b010001;
    assign _1849 = _46[7:7];
    assign _1848 = _46[7:7];
    assign _1847 = _46[7:7];
    assign _1843 = _1130[7:7];
    assign _1842 = _1112[7:7];
    assign _1841 = _1105[7:7];
    assign _1840 = _1095[7:7];
    assign _1839 = _1088[7:7];
    assign _1074 = _1073[7:0];
    assign _1838 = _1074[7:7];
    assign _1837 = execute_result[7:7];
    assign _1836 = execute_result[7:7];
    assign _1835 = execute_result[7:7];
    assign _1834 = execute_result[7:7];
    assign _1833 = execute_result[7:7];
    assign _1832 = execute_result[7:7];
    assign _1831 = _1044[7:7];
    assign _1830 = _1033[7:7];
    assign _1829 = _1024[7:7];
    assign _1828 = and_[7:7];
    assign _1827 = _1015[7:7];
    assign _1826 = _1003[7:7];
    assign _1825 = operand[7:7];
    assign _1824 = operand[7:7];
    assign _1823 = operand[7:7];
    assign _1822 = operand[7:7];
    assign _1821 = _987[7:7];
    assign _983 = _982[7:0];
    assign _1820 = _983[7:7];
    assign _973 = _972[7:0];
    assign _1819 = _973[7:7];
    assign _963 = _962[7:0];
    assign _1818 = _963[7:7];
    assign _1817 = operand[7:7];
    assign _1816 = and_[7:7];
    assign _1815 = _946[7:7];
    always @* begin
        case (op$binary_variant)
        6'b000001:
            _1844 <= _1815;
        6'b000010:
            _1844 <= _1816;
        6'b000011:
            _1844 <= _1817;
        6'b000100:
            _1844 <= _1818;
        6'b000101:
            _1844 <= _1819;
        6'b000110:
            _1844 <= _1820;
        6'b000111:
            _1844 <= _1821;
        6'b001000:
            _1844 <= _1822;
        6'b001001:
            _1844 <= _1823;
        6'b001010:
            _1844 <= _1824;
        6'b001011:
            _1844 <= _1825;
        6'b001100:
            _1844 <= _1826;
        6'b001101:
            _1844 <= _1827;
        6'b001110:
            _1844 <= _1828;
        6'b001111:
            _1844 <= _1829;
        6'b010000:
            _1844 <= _1830;
        6'b010001:
            _1844 <= _1831;
        6'b010110:
            _1844 <= _1832;
        6'b010111:
            _1844 <= _1833;
        6'b011000:
            _1844 <= _1834;
        6'b011001:
            _1844 <= _1835;
        6'b011010:
            _1844 <= _1836;
        6'b011011:
            _1844 <= _1837;
        6'b011100:
            _1844 <= _1838;
        6'b011101:
            _1844 <= _1839;
        6'b011110:
            _1844 <= _1840;
        6'b011111:
            _1844 <= _1841;
        6'b100000:
            _1844 <= _1842;
        6'b100001:
            _1844 <= _1843;
        default:
            _1844 <= flags$n;
        endcase
    end
    assign _1845 = execute_unit_enable ? _1844 : flags$n;
    assign _1814 = _933[7:7];
    assign _1813 = _928[7:7];
    assign _1812 = _923[7:7];
    assign _1811 = regs$s[7:7];
    assign _1810 = regs$a[7:7];
    assign _1809 = regs$a[7:7];
    assign _1808 = regs$y[7:7];
    assign _1807 = regs$x[7:7];
    always @* begin
        case (op$binary_variant)
        6'b001011:
            _1802 <= operand;
        default:
            _1802 <= regs$y;
        endcase
    end
    assign _1803 = execute_unit_enable ? _1802 : regs$y;
    assign _923 = regs$y + _1917;
    assign _1130 = _1129[7:0];
    assign _1112 = regs$a ^ _1111;
    assign _1105 = _1104 & regs$a;
    assign _1095 = regs$a | _1094;
    assign _1088 = _1087[7:0];
    assign _1023 = and_[7:1];
    assign _1024 = { _91,
                     _1023 };
    assign _1015 = _1014[7:0];
    assign _1003 = regs$a | operand;
    assign _987 = regs$a ^ operand;
    assign _946 = _945[7:0];
    always @* begin
        case (op$binary_variant)
        6'b000001:
            _1798 <= _946;
        6'b000010:
            _1798 <= and_;
        6'b000111:
            _1798 <= _987;
        6'b001000:
            _1798 <= operand;
        6'b001001:
            _1798 <= operand;
        6'b001100:
            _1798 <= _1003;
        6'b001101:
            _1798 <= _1015;
        6'b001110:
            _1798 <= and_;
        6'b001111:
            _1798 <= _1024;
        6'b010000:
            _1798 <= _1033;
        6'b011101:
            _1798 <= _1088;
        6'b011110:
            _1798 <= _1095;
        6'b011111:
            _1798 <= _1105;
        6'b100000:
            _1798 <= _1112;
        6'b100001:
            _1798 <= _1130;
        default:
            _1798 <= regs$a;
        endcase
    end
    assign _1799 = execute_unit_enable ? _1798 : regs$a;
    assign _1193 = _46 + _1917;
    assign _1189 = regs$a & regs$x;
    assign _1188 = 8'b11111111;
    always @* begin
        case (op$binary_variant)
        0:
            _1190 <= _1188;
        1:
            _1190 <= _1188;
        2:
            _1190 <= _1188;
        3:
            _1190 <= _1188;
        4:
            _1190 <= _1188;
        5:
            _1190 <= _1188;
        6:
            _1190 <= _1188;
        7:
            _1190 <= _1188;
        8:
            _1190 <= _1188;
        9:
            _1190 <= _1188;
        10:
            _1190 <= _1188;
        11:
            _1190 <= _1188;
        12:
            _1190 <= _1188;
        13:
            _1190 <= _1188;
        14:
            _1190 <= _1188;
        15:
            _1190 <= _1188;
        16:
            _1190 <= _1188;
        17:
            _1190 <= _1188;
        18:
            _1190 <= _1188;
        19:
            _1190 <= _1188;
        20:
            _1190 <= _1188;
        21:
            _1190 <= _1188;
        22:
            _1190 <= _1188;
        23:
            _1190 <= _1188;
        24:
            _1190 <= _1188;
        25:
            _1190 <= _1188;
        26:
            _1190 <= _1188;
        27:
            _1190 <= _1188;
        28:
            _1190 <= _1188;
        29:
            _1190 <= _1188;
        30:
            _1190 <= _1188;
        31:
            _1190 <= _1188;
        32:
            _1190 <= _1188;
        33:
            _1190 <= _1188;
        34:
            _1190 <= _1189;
        35:
            _1190 <= regs$x;
        default:
            _1190 <= regs$y;
        endcase
    end
    assign _1187 = _46 + _1917;
    assign _1191 = _1187 & _1190;
    always @* begin
        case (state_0)
        6'b001010:
            _1194 <= _1191;
        6'b010000:
            _1194 <= _1193;
        default:
            _1194 <= h;
        endcase
    end
    assign _22 = _1194;
    always @(posedge _44) begin
        if (_42)
            h <= _121;
        else
            if (_40)
                h <= _22;
    end
    assign _1110 = operand[7:1];
    assign _1111 = { _91,
                     _1110 };
    assign _1102 = 7'b0000000;
    assign _1103 = { _1102,
                     flags$c };
    assign _1099 = operand[6:0];
    assign _1101 = { _1099,
                     _91 };
    assign _1104 = _1101 | _1103;
    assign _1092 = operand[6:0];
    assign _1094 = { _1092,
                     _91 };
    assign _1791 = { flags$c,
                     _1102 };
    assign _1788 = operand[7:1];
    assign _1789 = { _91,
                     _1788 };
    assign _1792 = _1789 | _1791;
    assign _1241 = _46[0:0];
    assign _1240 = _46[0:0];
    assign _1127 = operand[0:0];
    assign _1126 = 9'b000000000;
    assign _1128 = { _1126,
                     _1127 };
    assign _1121 = { flags$c,
                     _1102 };
    assign _1118 = operand[7:1];
    assign _1119 = { _91,
                     _1118 };
    assign _1122 = _1119 | _1121;
    assign _1123 = { gnd,
                     _1122 };
    assign _1116 = { gnd,
                     regs$a };
    assign _1124 = _1116 + _1123;
    assign _1125 = { gnd,
                     _1124 };
    assign _1129 = _1125 + _1128;
    assign _1233 = _1129[9:8];
    assign _1235 = _1233 == _127;
    assign _1236 = ~ _1235;
    assign _1232 = operand[0:0];
    assign _1231 = operand[7:7];
    assign _1230 = operand[7:7];
    assign _1086 = { _1126,
                     flags$c };
    assign _1080 = operand + _1917;
    assign _1081 = ~ _1080;
    assign _1082 = { gnd,
                     _1081 };
    assign _1078 = { gnd,
                     regs$a };
    assign _1083 = _1078 + _1082;
    assign _1084 = { gnd,
                     _1083 };
    assign _1087 = _1084 + _1086;
    assign _1226 = _1087[9:8];
    assign _1228 = _1226 == _127;
    assign _1229 = ~ _1228;
    assign _1072 = { gnd,
                     regs$a };
    assign _1070 = 9'b000000001;
    assign _1067 = operand - _1917;
    assign _1068 = ~ _1067;
    assign _1069 = { gnd,
                     _1068 };
    assign _1071 = _1069 + _1070;
    assign _1073 = _1071 + _1072;
    assign _1223 = ~ _1073;
    assign _1224 = _1223[8:8];
    assign _1225 = ~ _1224;
    assign _1222 = operand[0:0];
    assign _1221 = operand[7:7];
    assign _1220 = operand[0:0];
    assign _1219 = operand[7:7];
    assign _1216 = ~ _1043;
    assign _1217 = _1216[8:8];
    assign _1218 = ~ _1217;
    assign _1032 = { flags$c,
                     _1102 };
    assign _1029 = and_[7:1];
    assign _1030 = { _91,
                     _1029 };
    assign _1033 = _1030 | _1032;
    assign _1215 = _1033[6:6];
    assign and_ = regs$a & operand;
    assign _1214 = and_[7:7];
    assign _1013 = { _1126,
                     flags$c };
    assign _1008 = ~ operand;
    assign _1009 = { gnd,
                     _1008 };
    assign _1007 = { gnd,
                     regs$a };
    assign _1010 = _1007 + _1009;
    assign _1011 = { gnd,
                     _1010 };
    assign _1014 = _1011 + _1013;
    assign _1210 = _1014[9:8];
    assign _1212 = _1210 == _127;
    assign _1213 = ~ _1212;
    assign _981 = { gnd,
                    regs$y };
    assign _977 = ~ operand;
    assign _978 = { gnd,
                    _977 };
    assign _980 = _978 + _1070;
    assign _982 = _980 + _981;
    assign _1207 = ~ _982;
    assign _1208 = _1207[8:8];
    assign _1209 = ~ _1208;
    assign _971 = { gnd,
                    regs$x };
    assign _967 = ~ operand;
    assign _968 = { gnd,
                    _967 };
    assign _970 = _968 + _1070;
    assign _972 = _970 + _971;
    assign _1204 = ~ _972;
    assign _1205 = _1204[8:8];
    assign _1206 = ~ _1205;
    assign _961 = { gnd,
                    regs$a };
    assign _957 = ~ operand;
    assign _958 = { gnd,
                    _957 };
    assign _960 = _958 + _1070;
    assign _962 = _960 + _961;
    assign _1201 = ~ _962;
    assign _1202 = _1201[8:8];
    assign _1203 = ~ _1202;
    assign _944 = { _1126,
                    flags$c };
    assign _940 = { gnd,
                    operand };
    assign _939 = { gnd,
                    regs$a };
    assign _941 = _939 + _940;
    assign _942 = { gnd,
                    _941 };
    assign _945 = _942 + _944;
    assign _1197 = _945[9:8];
    assign _1199 = _1197 == _127;
    assign _1200 = ~ _1199;
    always @* begin
        case (op$binary_variant)
        6'b000001:
            _1237 <= _1200;
        6'b000100:
            _1237 <= _1203;
        6'b000101:
            _1237 <= _1206;
        6'b000110:
            _1237 <= _1209;
        6'b001101:
            _1237 <= _1213;
        6'b001110:
            _1237 <= _1214;
        6'b010000:
            _1237 <= _1215;
        6'b010001:
            _1237 <= _1218;
        6'b010110:
            _1237 <= _1219;
        6'b011001:
            _1237 <= _1220;
        6'b011010:
            _1237 <= _1221;
        6'b011011:
            _1237 <= _1222;
        6'b011100:
            _1237 <= _1225;
        6'b011101:
            _1237 <= _1229;
        6'b011110:
            _1237 <= _1230;
        6'b011111:
            _1237 <= _1231;
        6'b100000:
            _1237 <= _1232;
        6'b100001:
            _1237 <= _1236;
        default:
            _1237 <= flags$c;
        endcase
    end
    assign _1238 = execute_unit_enable ? _1237 : flags$c;
    always @* begin
        case (instr)
        8'b00011000:
            _1239 <= gnd;
        8'b00111000:
            _1239 <= vdd;
        default:
            _1239 <= _1238;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000011:
            _1242 <= _1239;
        6'b100000:
            _1242 <= _1240;
        6'b101000:
            _1242 <= _1241;
        default:
            _1242 <= _1238;
        endcase
    end
    assign _23 = _1242;
    always @(posedge _44) begin
        if (_42)
            flags$c <= _91;
        else
            if (_40)
                flags$c <= _23;
    end
    assign _1785 = { _1102,
                     flags$c };
    assign _1781 = operand[6:0];
    assign _1783 = { _1781,
                     _91 };
    assign _1786 = _1783 | _1785;
    assign _1779 = operand[7:1];
    assign _1780 = { _91,
                     _1779 };
    assign _1777 = operand + _1917;
    assign _1775 = operand - _1917;
    assign _1771 = operand[6:0];
    assign _1773 = { _1771,
                     _91 };
    assign _1769 = regs$a & regs$x;
    always @* begin
        case (op$binary_variant)
        6'b010010:
            _1796 <= regs$a;
        6'b010011:
            _1796 <= regs$x;
        6'b010100:
            _1796 <= regs$y;
        6'b010101:
            _1796 <= _1769;
        6'b010110:
            _1796 <= _1773;
        6'b010111:
            _1796 <= _1775;
        6'b011000:
            _1796 <= _1777;
        6'b011001:
            _1796 <= _1780;
        6'b011010:
            _1796 <= _1786;
        6'b011011:
            _1796 <= _1792;
        6'b011100:
            _1796 <= _1067;
        6'b011101:
            _1796 <= _1080;
        6'b011110:
            _1796 <= _1094;
        6'b011111:
            _1796 <= _1104;
        6'b100000:
            _1796 <= _1111;
        6'b100001:
            _1796 <= _1122;
        6'b100010:
            _1796 <= h;
        6'b100011:
            _1796 <= h;
        6'b100100:
            _1796 <= h;
        default:
            _1796 <= _1765;
        endcase
    end
    assign _24 = execute_result;
    always @(posedge _44) begin
        if (_42)
            _1765 <= _121;
        else
            if (_40)
                _1765 <= _24;
    end
    assign w = _1749[1:1];
    assign _1759 = w ? vdd : gnd;
    assign _1754 = ~ index_carry;
    assign _1755 = r & _1754;
    assign _1756 = _1755 ? vdd : gnd;
    assign _25 = index_carry;
    always @(posedge _44) begin
        if (_42)
            _1309 <= _91;
        else
            if (_40)
                _1309 <= _25;
    end
    assign _1307 = _1301[8:8];
    assign _1296 = { gnd,
                     index_reg };
    assign _1304 = addr_low + _1917;
    assign _1300 = { gnd,
                     index_reg };
    assign _1299 = { gnd,
                     addr_low };
    assign _1301 = _1299 + _1300;
    assign _1302 = _1301[7:0];
    assign _1298 = _1297[7:0];
    assign _1041 = regs$a & regs$x;
    assign _1042 = { gnd,
                     _1041 };
    assign _1037 = ~ operand;
    assign _1038 = { gnd,
                     _1037 };
    assign _1040 = _1038 + _1070;
    assign _1043 = _1040 + _1042;
    assign _1044 = _1043[7:0];
    always @* begin
        case (instr)
        8'b00001010:
            _1247 <= regs$a;
        8'b00101010:
            _1247 <= regs$a;
        8'b01001010:
            _1247 <= regs$a;
        8'b01101010:
            _1247 <= regs$a;
        default:
            _1247 <= _46;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000011:
            _1248 <= _1247;
        default:
            _1248 <= _46;
        endcase
    end
    assign operand = _1248;
    always @* begin
        case (op$binary_variant)
        6'b001000:
            _1284 <= operand;
        6'b001010:
            _1284 <= operand;
        6'b010001:
            _1284 <= _1044;
        default:
            _1284 <= regs$x;
        endcase
    end
    assign _1285 = execute_unit_enable ? _1284 : regs$x;
    assign _933 = regs$x + _1917;
    assign _928 = regs$x - _1917;
    assign _1282 = regs$s - _1917;
    assign _1280 = regs$s - _1917;
    assign _1278 = regs$s - _1917;
    assign _1275 = regs$s + _1917;
    assign _1272 = regs$s + _1917;
    assign _1270 = regs$s - _1917;
    assign _1268 = regs$s - _1917;
    assign _1266 = regs$s + _1917;
    assign _1264 = regs$s + _1917;
    assign _1261 = regs$s + _1917;
    assign _1259 = regs$s + _1917;
    assign _1256 = regs$s + _1917;
    assign _1254 = regs$s - _1917;
    assign _1252 = regs$s - _1917;
    always @* begin
        case (instr)
        8'b10011010:
            _1250 <= regs$x;
        default:
            _1250 <= regs$s;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000011:
            _1283 <= _1250;
        6'b011000:
            _1283 <= _1252;
        6'b011001:
            _1283 <= _1254;
        6'b011011:
            _1283 <= _1256;
        6'b011100:
            _1283 <= _1259;
        6'b011111:
            _1283 <= _1261;
        6'b100000:
            _1283 <= _1264;
        6'b100001:
            _1283 <= _1266;
        6'b100011:
            _1283 <= _1268;
        6'b100100:
            _1283 <= _1270;
        6'b100101:
            _1283 <= _1272;
        6'b100111:
            _1283 <= _1275;
        6'b101010:
            _1283 <= _1278;
        6'b101011:
            _1283 <= _1280;
        6'b101100:
            _1283 <= _1282;
        default:
            _1283 <= regs$s;
        endcase
    end
    assign _27 = _1283;
    always @(posedge _44) begin
        if (_42)
            regs$s <= _121;
        else
            if (_40)
                regs$s <= _27;
    end
    always @* begin
        case (instr)
        8'b10101010:
            _1286 <= regs$a;
        8'b10111010:
            _1286 <= regs$s;
        8'b11001010:
            _1286 <= _928;
        8'b11101000:
            _1286 <= _933;
        default:
            _1286 <= _1285;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000011:
            _1287 <= _1286;
        default:
            _1287 <= _1285;
        endcase
    end
    assign _28 = _1287;
    always @(posedge _44) begin
        if (_42)
            regs$x <= _121;
        else
            if (_40)
                regs$x <= _28;
    end
    always @* begin
        case (instr)
        8'b00010001:
            _1290 <= regs$y;
        8'b00010011:
            _1290 <= regs$y;
        8'b00010100:
            _1290 <= regs$x;
        8'b00010101:
            _1290 <= regs$x;
        8'b00010110:
            _1290 <= regs$x;
        8'b00010111:
            _1290 <= regs$x;
        8'b00011001:
            _1290 <= regs$y;
        8'b00011011:
            _1290 <= regs$y;
        8'b00011100:
            _1290 <= regs$x;
        8'b00011101:
            _1290 <= regs$x;
        8'b00011110:
            _1290 <= regs$x;
        8'b00011111:
            _1290 <= regs$x;
        8'b00110001:
            _1290 <= regs$y;
        8'b00110011:
            _1290 <= regs$y;
        8'b00110100:
            _1290 <= regs$x;
        8'b00110101:
            _1290 <= regs$x;
        8'b00110110:
            _1290 <= regs$x;
        8'b00110111:
            _1290 <= regs$x;
        8'b00111001:
            _1290 <= regs$y;
        8'b00111011:
            _1290 <= regs$y;
        8'b00111100:
            _1290 <= regs$x;
        8'b00111101:
            _1290 <= regs$x;
        8'b00111110:
            _1290 <= regs$x;
        8'b00111111:
            _1290 <= regs$x;
        8'b01010001:
            _1290 <= regs$y;
        8'b01010011:
            _1290 <= regs$y;
        8'b01010100:
            _1290 <= regs$x;
        8'b01010101:
            _1290 <= regs$x;
        8'b01010110:
            _1290 <= regs$x;
        8'b01010111:
            _1290 <= regs$x;
        8'b01011001:
            _1290 <= regs$y;
        8'b01011011:
            _1290 <= regs$y;
        8'b01011100:
            _1290 <= regs$x;
        8'b01011101:
            _1290 <= regs$x;
        8'b01011110:
            _1290 <= regs$x;
        8'b01011111:
            _1290 <= regs$x;
        8'b01110001:
            _1290 <= regs$y;
        8'b01110011:
            _1290 <= regs$y;
        8'b01110100:
            _1290 <= regs$x;
        8'b01110101:
            _1290 <= regs$x;
        8'b01110110:
            _1290 <= regs$x;
        8'b01110111:
            _1290 <= regs$x;
        8'b01111001:
            _1290 <= regs$y;
        8'b01111011:
            _1290 <= regs$y;
        8'b01111100:
            _1290 <= regs$x;
        8'b01111101:
            _1290 <= regs$x;
        8'b01111110:
            _1290 <= regs$x;
        8'b01111111:
            _1290 <= regs$x;
        8'b10010001:
            _1290 <= regs$y;
        8'b10010011:
            _1290 <= regs$y;
        8'b10010100:
            _1290 <= regs$x;
        8'b10010101:
            _1290 <= regs$x;
        8'b10010110:
            _1290 <= regs$y;
        8'b10010111:
            _1290 <= regs$y;
        8'b10011001:
            _1290 <= regs$y;
        8'b10011100:
            _1290 <= regs$x;
        8'b10011101:
            _1290 <= regs$x;
        8'b10011110:
            _1290 <= regs$y;
        8'b10011111:
            _1290 <= regs$y;
        8'b10110001:
            _1290 <= regs$y;
        8'b10110011:
            _1290 <= regs$y;
        8'b10110100:
            _1290 <= regs$x;
        8'b10110101:
            _1290 <= regs$x;
        8'b10110110:
            _1290 <= regs$y;
        8'b10110111:
            _1290 <= regs$y;
        8'b10111001:
            _1290 <= regs$y;
        8'b10111100:
            _1290 <= regs$x;
        8'b10111101:
            _1290 <= regs$x;
        8'b10111110:
            _1290 <= regs$y;
        8'b10111111:
            _1290 <= regs$y;
        8'b11010001:
            _1290 <= regs$y;
        8'b11010011:
            _1290 <= regs$y;
        8'b11010100:
            _1290 <= regs$x;
        8'b11010101:
            _1290 <= regs$x;
        8'b11010110:
            _1290 <= regs$x;
        8'b11010111:
            _1290 <= regs$x;
        8'b11011001:
            _1290 <= regs$y;
        8'b11011011:
            _1290 <= regs$y;
        8'b11011100:
            _1290 <= regs$x;
        8'b11011101:
            _1290 <= regs$x;
        8'b11011110:
            _1290 <= regs$x;
        8'b11011111:
            _1290 <= regs$x;
        8'b11110001:
            _1290 <= regs$y;
        8'b11110011:
            _1290 <= regs$y;
        8'b11110100:
            _1290 <= regs$x;
        8'b11110101:
            _1290 <= regs$x;
        8'b11110110:
            _1290 <= regs$x;
        8'b11110111:
            _1290 <= regs$x;
        8'b11111001:
            _1290 <= regs$y;
        8'b11111011:
            _1290 <= regs$y;
        8'b11111100:
            _1290 <= regs$x;
        8'b11111101:
            _1290 <= regs$x;
        8'b11111110:
            _1290 <= regs$x;
        8'b11111111:
            _1290 <= regs$x;
        default:
            _1290 <= index_reg;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000011:
            _1291 <= _1290;
        default:
            _1291 <= index_reg;
        endcase
    end
    assign _29 = _1291;
    always @(posedge _44) begin
        if (_42)
            index_reg <= _121;
        else
            if (_40)
                index_reg <= _29;
    end
    assign _1293 = addr_low + index_reg;
    always @* begin
        case (instr)
        8'b00000100:
            _1292 <= _46;
        8'b00000101:
            _1292 <= _46;
        8'b00000110:
            _1292 <= _46;
        8'b00000111:
            _1292 <= _46;
        8'b00001100:
            _1292 <= _46;
        8'b00001101:
            _1292 <= _46;
        8'b00001110:
            _1292 <= _46;
        8'b00001111:
            _1292 <= _46;
        8'b00010100:
            _1292 <= _46;
        8'b00010101:
            _1292 <= _46;
        8'b00010110:
            _1292 <= _46;
        8'b00010111:
            _1292 <= _46;
        8'b00011001:
            _1292 <= _46;
        8'b00011011:
            _1292 <= _46;
        8'b00011100:
            _1292 <= _46;
        8'b00011101:
            _1292 <= _46;
        8'b00011110:
            _1292 <= _46;
        8'b00011111:
            _1292 <= _46;
        8'b00100000:
            _1292 <= _46;
        8'b00100100:
            _1292 <= _46;
        8'b00100101:
            _1292 <= _46;
        8'b00100110:
            _1292 <= _46;
        8'b00100111:
            _1292 <= _46;
        8'b00101100:
            _1292 <= _46;
        8'b00101101:
            _1292 <= _46;
        8'b00101110:
            _1292 <= _46;
        8'b00101111:
            _1292 <= _46;
        8'b00110100:
            _1292 <= _46;
        8'b00110101:
            _1292 <= _46;
        8'b00110110:
            _1292 <= _46;
        8'b00110111:
            _1292 <= _46;
        8'b00111001:
            _1292 <= _46;
        8'b00111011:
            _1292 <= _46;
        8'b00111100:
            _1292 <= _46;
        8'b00111101:
            _1292 <= _46;
        8'b00111110:
            _1292 <= _46;
        8'b00111111:
            _1292 <= _46;
        8'b01000100:
            _1292 <= _46;
        8'b01000101:
            _1292 <= _46;
        8'b01000110:
            _1292 <= _46;
        8'b01000111:
            _1292 <= _46;
        8'b01001101:
            _1292 <= _46;
        8'b01001110:
            _1292 <= _46;
        8'b01001111:
            _1292 <= _46;
        8'b01010100:
            _1292 <= _46;
        8'b01010101:
            _1292 <= _46;
        8'b01010110:
            _1292 <= _46;
        8'b01010111:
            _1292 <= _46;
        8'b01011001:
            _1292 <= _46;
        8'b01011011:
            _1292 <= _46;
        8'b01011100:
            _1292 <= _46;
        8'b01011101:
            _1292 <= _46;
        8'b01011110:
            _1292 <= _46;
        8'b01011111:
            _1292 <= _46;
        8'b01100100:
            _1292 <= _46;
        8'b01100101:
            _1292 <= _46;
        8'b01100110:
            _1292 <= _46;
        8'b01100111:
            _1292 <= _46;
        8'b01101100:
            _1292 <= _46;
        8'b01101101:
            _1292 <= _46;
        8'b01101110:
            _1292 <= _46;
        8'b01101111:
            _1292 <= _46;
        8'b01110100:
            _1292 <= _46;
        8'b01110101:
            _1292 <= _46;
        8'b01110110:
            _1292 <= _46;
        8'b01110111:
            _1292 <= _46;
        8'b01111001:
            _1292 <= _46;
        8'b01111011:
            _1292 <= _46;
        8'b01111100:
            _1292 <= _46;
        8'b01111101:
            _1292 <= _46;
        8'b01111110:
            _1292 <= _46;
        8'b01111111:
            _1292 <= _46;
        8'b10000100:
            _1292 <= _46;
        8'b10000101:
            _1292 <= _46;
        8'b10000110:
            _1292 <= _46;
        8'b10000111:
            _1292 <= _46;
        8'b10001100:
            _1292 <= _46;
        8'b10001101:
            _1292 <= _46;
        8'b10001110:
            _1292 <= _46;
        8'b10001111:
            _1292 <= _46;
        8'b10010100:
            _1292 <= _46;
        8'b10010101:
            _1292 <= _46;
        8'b10010110:
            _1292 <= _46;
        8'b10010111:
            _1292 <= _46;
        8'b10011001:
            _1292 <= _46;
        8'b10011100:
            _1292 <= _46;
        8'b10011101:
            _1292 <= _46;
        8'b10011110:
            _1292 <= _46;
        8'b10011111:
            _1292 <= _46;
        8'b10100100:
            _1292 <= _46;
        8'b10100101:
            _1292 <= _46;
        8'b10100110:
            _1292 <= _46;
        8'b10100111:
            _1292 <= _46;
        8'b10101100:
            _1292 <= _46;
        8'b10101101:
            _1292 <= _46;
        8'b10101110:
            _1292 <= _46;
        8'b10101111:
            _1292 <= _46;
        8'b10110100:
            _1292 <= _46;
        8'b10110101:
            _1292 <= _46;
        8'b10110110:
            _1292 <= _46;
        8'b10110111:
            _1292 <= _46;
        8'b10111001:
            _1292 <= _46;
        8'b10111100:
            _1292 <= _46;
        8'b10111101:
            _1292 <= _46;
        8'b10111110:
            _1292 <= _46;
        8'b10111111:
            _1292 <= _46;
        8'b11000100:
            _1292 <= _46;
        8'b11000101:
            _1292 <= _46;
        8'b11000110:
            _1292 <= _46;
        8'b11000111:
            _1292 <= _46;
        8'b11001100:
            _1292 <= _46;
        8'b11001101:
            _1292 <= _46;
        8'b11001110:
            _1292 <= _46;
        8'b11001111:
            _1292 <= _46;
        8'b11010100:
            _1292 <= _46;
        8'b11010101:
            _1292 <= _46;
        8'b11010110:
            _1292 <= _46;
        8'b11010111:
            _1292 <= _46;
        8'b11011001:
            _1292 <= _46;
        8'b11011011:
            _1292 <= _46;
        8'b11011100:
            _1292 <= _46;
        8'b11011101:
            _1292 <= _46;
        8'b11011110:
            _1292 <= _46;
        8'b11011111:
            _1292 <= _46;
        8'b11100100:
            _1292 <= _46;
        8'b11100101:
            _1292 <= _46;
        8'b11100110:
            _1292 <= _46;
        8'b11100111:
            _1292 <= _46;
        8'b11101100:
            _1292 <= _46;
        8'b11101101:
            _1292 <= _46;
        8'b11101110:
            _1292 <= _46;
        8'b11101111:
            _1292 <= _46;
        8'b11110100:
            _1292 <= _46;
        8'b11110101:
            _1292 <= _46;
        8'b11110110:
            _1292 <= _46;
        8'b11110111:
            _1292 <= _46;
        8'b11111001:
            _1292 <= _46;
        8'b11111011:
            _1292 <= _46;
        8'b11111100:
            _1292 <= _46;
        8'b11111101:
            _1292 <= _46;
        8'b11111110:
            _1292 <= _46;
        8'b11111111:
            _1292 <= _46;
        default:
            _1292 <= addr_low;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000011:
            _1305 <= _1292;
        6'b001001:
            _1305 <= _1293;
        6'b001010:
            _1305 <= _1298;
        6'b001101:
            _1305 <= _46;
        6'b001111:
            _1305 <= _46;
        6'b010000:
            _1305 <= _1302;
        6'b010101:
            _1305 <= _1304;
        default:
            _1305 <= addr_low;
        endcase
    end
    assign _30 = _1305;
    always @(posedge _44) begin
        if (_42)
            addr_low <= _121;
        else
            if (_40)
                addr_low <= _30;
    end
    assign _1295 = { gnd,
                     addr_low };
    assign _1297 = _1295 + _1296;
    assign _1306 = _1297[8:8];
    always @* begin
        case (state_0)
        6'b001010:
            _1310 <= _1306;
        6'b010000:
            _1310 <= _1307;
        default:
            _1310 <= _1309;
        endcase
    end
    assign index_carry = _1310;
    assign _1751 = ~ index_carry;
    assign _1748 = 3'b010;
    assign _1745 = 3'b001;
    assign _1729 = 3'b100;
    assign _1603 = 6'b001000;
    assign _1547 = 6'b100100;
    assign _1501 = 6'b000001;
    assign _1405 = 6'b000010;
    assign _1379 = 6'b000011;
    assign _1355 = 6'b001100;
    always @* begin
        case (_46)
        8'b00000001:
            _1709 <= _1355;
        8'b00000011:
            _1709 <= _891;
        8'b00000100:
            _1709 <= _52;
        8'b00000101:
            _1709 <= _1355;
        8'b00000110:
            _1709 <= _882;
        8'b00000111:
            _1709 <= _891;
        8'b00001001:
            _1709 <= _1355;
        8'b00001010:
            _1709 <= _882;
        8'b00001011:
            _1709 <= _268;
        8'b00001100:
            _1709 <= _52;
        8'b00001101:
            _1709 <= _1355;
        8'b00001110:
            _1709 <= _882;
        8'b00001111:
            _1709 <= _891;
        8'b00010001:
            _1709 <= _1355;
        8'b00010011:
            _1709 <= _891;
        8'b00010100:
            _1709 <= _52;
        8'b00010101:
            _1709 <= _1355;
        8'b00010110:
            _1709 <= _882;
        8'b00010111:
            _1709 <= _891;
        8'b00011001:
            _1709 <= _1355;
        8'b00011011:
            _1709 <= _891;
        8'b00011100:
            _1709 <= _52;
        8'b00011101:
            _1709 <= _1355;
        8'b00011110:
            _1709 <= _882;
        8'b00011111:
            _1709 <= _891;
        8'b00100001:
            _1709 <= _1405;
        8'b00100011:
            _1709 <= _1262;
        8'b00100100:
            _1709 <= _1379;
        8'b00100101:
            _1709 <= _1405;
        8'b00100110:
            _1709 <= _886;
        8'b00100111:
            _1709 <= _1262;
        8'b00101001:
            _1709 <= _1405;
        8'b00101010:
            _1709 <= _886;
        8'b00101011:
            _1709 <= _268;
        8'b00101100:
            _1709 <= _1379;
        8'b00101101:
            _1709 <= _1405;
        8'b00101110:
            _1709 <= _886;
        8'b00101111:
            _1709 <= _1262;
        8'b00110001:
            _1709 <= _1405;
        8'b00110011:
            _1709 <= _1262;
        8'b00110100:
            _1709 <= _52;
        8'b00110101:
            _1709 <= _1405;
        8'b00110110:
            _1709 <= _886;
        8'b00110111:
            _1709 <= _1262;
        8'b00111001:
            _1709 <= _1405;
        8'b00111011:
            _1709 <= _1262;
        8'b00111100:
            _1709 <= _52;
        8'b00111101:
            _1709 <= _1405;
        8'b00111110:
            _1709 <= _886;
        8'b00111111:
            _1709 <= _1262;
        8'b01000001:
            _1709 <= _83;
        8'b01000011:
            _1709 <= _101;
        8'b01000100:
            _1709 <= _52;
        8'b01000101:
            _1709 <= _83;
        8'b01000110:
            _1709 <= _85;
        8'b01000111:
            _1709 <= _101;
        8'b01001001:
            _1709 <= _83;
        8'b01001010:
            _1709 <= _85;
        8'b01001011:
            _1709 <= _339;
        8'b01001101:
            _1709 <= _83;
        8'b01001110:
            _1709 <= _85;
        8'b01001111:
            _1709 <= _101;
        8'b01010001:
            _1709 <= _83;
        8'b01010011:
            _1709 <= _101;
        8'b01010100:
            _1709 <= _52;
        8'b01010101:
            _1709 <= _83;
        8'b01010110:
            _1709 <= _85;
        8'b01010111:
            _1709 <= _101;
        8'b01011001:
            _1709 <= _83;
        8'b01011011:
            _1709 <= _101;
        8'b01011100:
            _1709 <= _52;
        8'b01011101:
            _1709 <= _83;
        8'b01011110:
            _1709 <= _85;
        8'b01011111:
            _1709 <= _101;
        8'b01100001:
            _1709 <= _1501;
        8'b01100011:
            _1709 <= _355;
        8'b01100100:
            _1709 <= _52;
        8'b01100101:
            _1709 <= _1501;
        8'b01100110:
            _1709 <= _1257;
        8'b01100111:
            _1709 <= _355;
        8'b01101001:
            _1709 <= _1501;
        8'b01101010:
            _1709 <= _1257;
        8'b01101011:
            _1709 <= _269;
        8'b01101101:
            _1709 <= _1501;
        8'b01101110:
            _1709 <= _1257;
        8'b01101111:
            _1709 <= _355;
        8'b01110001:
            _1709 <= _1501;
        8'b01110011:
            _1709 <= _355;
        8'b01110100:
            _1709 <= _52;
        8'b01110101:
            _1709 <= _1501;
        8'b01110110:
            _1709 <= _1257;
        8'b01110111:
            _1709 <= _355;
        8'b01111001:
            _1709 <= _1501;
        8'b01111011:
            _1709 <= _355;
        8'b01111100:
            _1709 <= _52;
        8'b01111101:
            _1709 <= _1501;
        8'b01111110:
            _1709 <= _1257;
        8'b01111111:
            _1709 <= _355;
        8'b10000000:
            _1709 <= _52;
        8'b10000001:
            _1709 <= _1899;
        8'b10000010:
            _1709 <= _52;
        8'b10000011:
            _1709 <= _353;
        8'b10000100:
            _1709 <= _270;
        8'b10000101:
            _1709 <= _1899;
        8'b10000110:
            _1709 <= _878;
        8'b10000111:
            _1709 <= _353;
        8'b10001001:
            _1709 <= _52;
        8'b10001100:
            _1709 <= _270;
        8'b10001101:
            _1709 <= _1899;
        8'b10001110:
            _1709 <= _878;
        8'b10001111:
            _1709 <= _353;
        8'b10010001:
            _1709 <= _1899;
        8'b10010011:
            _1709 <= _86;
        8'b10010100:
            _1709 <= _270;
        8'b10010101:
            _1709 <= _1899;
        8'b10010110:
            _1709 <= _878;
        8'b10010111:
            _1709 <= _353;
        8'b10011001:
            _1709 <= _1899;
        8'b10011100:
            _1709 <= _1547;
        8'b10011101:
            _1709 <= _1899;
        8'b10011110:
            _1709 <= _86;
        8'b10011111:
            _1709 <= _893;
        8'b10100000:
            _1709 <= _267;
        8'b10100001:
            _1709 <= _1294;
        8'b10100010:
            _1709 <= _263;
        8'b10100011:
            _1709 <= _1603;
        8'b10100100:
            _1709 <= _267;
        8'b10100101:
            _1709 <= _1294;
        8'b10100110:
            _1709 <= _263;
        8'b10100111:
            _1709 <= _1603;
        8'b10101001:
            _1709 <= _1294;
        8'b10101011:
            _1709 <= _1603;
        8'b10101100:
            _1709 <= _267;
        8'b10101101:
            _1709 <= _1294;
        8'b10101110:
            _1709 <= _263;
        8'b10101111:
            _1709 <= _1603;
        8'b10110001:
            _1709 <= _1294;
        8'b10110011:
            _1709 <= _1603;
        8'b10110100:
            _1709 <= _267;
        8'b10110101:
            _1709 <= _1294;
        8'b10110110:
            _1709 <= _263;
        8'b10110111:
            _1709 <= _1603;
        8'b10111001:
            _1709 <= _1294;
        8'b10111100:
            _1709 <= _267;
        8'b10111101:
            _1709 <= _1294;
        8'b10111110:
            _1709 <= _263;
        8'b10111111:
            _1709 <= _1603;
        8'b11000000:
            _1709 <= _1893;
        8'b11000001:
            _1709 <= _1762;
        8'b11000010:
            _1709 <= _52;
        8'b11000011:
            _1709 <= _354;
        8'b11000100:
            _1709 <= _1893;
        8'b11000101:
            _1709 <= _1762;
        8'b11000110:
            _1709 <= _1858;
        8'b11000111:
            _1709 <= _354;
        8'b11001001:
            _1709 <= _1762;
        8'b11001011:
            _1709 <= _352;
        8'b11001100:
            _1709 <= _1893;
        8'b11001101:
            _1709 <= _1762;
        8'b11001110:
            _1709 <= _1858;
        8'b11001111:
            _1709 <= _354;
        8'b11010001:
            _1709 <= _1762;
        8'b11010011:
            _1709 <= _354;
        8'b11010100:
            _1709 <= _52;
        8'b11010101:
            _1709 <= _1762;
        8'b11010110:
            _1709 <= _1858;
        8'b11010111:
            _1709 <= _354;
        8'b11011001:
            _1709 <= _1762;
        8'b11011011:
            _1709 <= _354;
        8'b11011100:
            _1709 <= _52;
        8'b11011101:
            _1709 <= _1762;
        8'b11011110:
            _1709 <= _1858;
        8'b11011111:
            _1709 <= _354;
        8'b11100000:
            _1709 <= _82;
        8'b11100001:
            _1709 <= _336;
        8'b11100010:
            _1709 <= _52;
        8'b11100011:
            _1709 <= _888;
        8'b11100100:
            _1709 <= _82;
        8'b11100101:
            _1709 <= _336;
        8'b11100110:
            _1709 <= _84;
        8'b11100111:
            _1709 <= _888;
        8'b11101001:
            _1709 <= _336;
        8'b11101011:
            _1709 <= _336;
        8'b11101100:
            _1709 <= _82;
        8'b11101101:
            _1709 <= _336;
        8'b11101110:
            _1709 <= _84;
        8'b11101111:
            _1709 <= _888;
        8'b11110001:
            _1709 <= _336;
        8'b11110011:
            _1709 <= _888;
        8'b11110100:
            _1709 <= _52;
        8'b11110101:
            _1709 <= _336;
        8'b11110110:
            _1709 <= _84;
        8'b11110111:
            _1709 <= _888;
        8'b11111001:
            _1709 <= _336;
        8'b11111011:
            _1709 <= _888;
        8'b11111100:
            _1709 <= _52;
        8'b11111101:
            _1709 <= _336;
        8'b11111110:
            _1709 <= _84;
        8'b11111111:
            _1709 <= _888;
        default:
            _1709 <= op$binary_variant;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000010:
            _1710 <= _1709;
        default:
            _1710 <= op$binary_variant;
        endcase
    end
    assign _32 = _1710;
    always @(posedge _44) begin
        if (_42)
            op$binary_variant <= _52;
        else
            if (_40)
                op$binary_variant <= _32;
    end
    always @* begin
        case (op$binary_variant)
        0:
            _1749 <= _1729;
        1:
            _1749 <= _1729;
        2:
            _1749 <= _1729;
        3:
            _1749 <= _1729;
        4:
            _1749 <= _1729;
        5:
            _1749 <= _1729;
        6:
            _1749 <= _1729;
        7:
            _1749 <= _1729;
        8:
            _1749 <= _1729;
        9:
            _1749 <= _1729;
        10:
            _1749 <= _1729;
        11:
            _1749 <= _1729;
        12:
            _1749 <= _1729;
        13:
            _1749 <= _1729;
        14:
            _1749 <= _1729;
        15:
            _1749 <= _1729;
        16:
            _1749 <= _1729;
        17:
            _1749 <= _1729;
        18:
            _1749 <= _1748;
        19:
            _1749 <= _1748;
        20:
            _1749 <= _1748;
        21:
            _1749 <= _1748;
        22:
            _1749 <= _1745;
        23:
            _1749 <= _1745;
        24:
            _1749 <= _1745;
        25:
            _1749 <= _1745;
        26:
            _1749 <= _1745;
        27:
            _1749 <= _1745;
        28:
            _1749 <= _1745;
        29:
            _1749 <= _1745;
        30:
            _1749 <= _1745;
        31:
            _1749 <= _1745;
        32:
            _1749 <= _1745;
        33:
            _1749 <= _1745;
        34:
            _1749 <= _1748;
        35:
            _1749 <= _1748;
        default:
            _1749 <= _1748;
        endcase
    end
    assign r = _1749[2:2];
    assign _1752 = r & _1751;
    assign _1753 = _1752 ? vdd : gnd;
    assign gnd = 1'b0;
    always @* begin
        case (instr)
        8'b00000100:
            _1711 <= vdd;
        8'b00000101:
            _1711 <= vdd;
        8'b00000110:
            _1711 <= vdd;
        8'b00000111:
            _1711 <= vdd;
        8'b00100100:
            _1711 <= vdd;
        8'b00100101:
            _1711 <= vdd;
        8'b00100110:
            _1711 <= vdd;
        8'b00100111:
            _1711 <= vdd;
        8'b01000100:
            _1711 <= vdd;
        8'b01000101:
            _1711 <= vdd;
        8'b01000110:
            _1711 <= vdd;
        8'b01000111:
            _1711 <= vdd;
        8'b01100100:
            _1711 <= vdd;
        8'b01100101:
            _1711 <= vdd;
        8'b01100110:
            _1711 <= vdd;
        8'b01100111:
            _1711 <= vdd;
        8'b10000100:
            _1711 <= vdd;
        8'b10000101:
            _1711 <= vdd;
        8'b10000110:
            _1711 <= vdd;
        8'b10000111:
            _1711 <= vdd;
        8'b10100100:
            _1711 <= vdd;
        8'b10100101:
            _1711 <= vdd;
        8'b10100110:
            _1711 <= vdd;
        8'b10100111:
            _1711 <= vdd;
        8'b11000100:
            _1711 <= vdd;
        8'b11000101:
            _1711 <= vdd;
        8'b11000110:
            _1711 <= vdd;
        8'b11000111:
            _1711 <= vdd;
        8'b11100100:
            _1711 <= vdd;
        8'b11100101:
            _1711 <= vdd;
        8'b11100110:
            _1711 <= vdd;
        8'b11100111:
            _1711 <= vdd;
        default:
            _1711 <= gnd;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000011:
            _1757 <= _1711;
        6'b001000:
            _1757 <= vdd;
        6'b001001:
            _1757 <= vdd;
        6'b001010:
            _1757 <= _1753;
        6'b001011:
            _1757 <= vdd;
        6'b001110:
            _1757 <= vdd;
        6'b010000:
            _1757 <= _1756;
        default:
            _1757 <= gnd;
        endcase
    end
    assign trigger_execute = _1757;
    assign _1760 = trigger_execute ? _1759 : gnd;
    assign vdd = 1'b1;
    always @* begin
        case (instr)
        8'b00001001:
            _1761 <= vdd;
        8'b00001010:
            _1761 <= vdd;
        8'b00001011:
            _1761 <= vdd;
        8'b00101001:
            _1761 <= vdd;
        8'b00101010:
            _1761 <= vdd;
        8'b00101011:
            _1761 <= vdd;
        8'b01001001:
            _1761 <= vdd;
        8'b01001010:
            _1761 <= vdd;
        8'b01001011:
            _1761 <= vdd;
        8'b01101001:
            _1761 <= vdd;
        8'b01101010:
            _1761 <= vdd;
        8'b01101011:
            _1761 <= vdd;
        8'b10000000:
            _1761 <= vdd;
        8'b10000010:
            _1761 <= vdd;
        8'b10001001:
            _1761 <= vdd;
        8'b10100000:
            _1761 <= vdd;
        8'b10100010:
            _1761 <= vdd;
        8'b10101001:
            _1761 <= vdd;
        8'b10101011:
            _1761 <= vdd;
        8'b11000000:
            _1761 <= vdd;
        8'b11000010:
            _1761 <= vdd;
        8'b11001001:
            _1761 <= vdd;
        8'b11001011:
            _1761 <= vdd;
        8'b11100000:
            _1761 <= vdd;
        8'b11100010:
            _1761 <= vdd;
        8'b11101001:
            _1761 <= vdd;
        8'b11101011:
            _1761 <= vdd;
        default:
            _1761 <= _1760;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000011:
            _1763 <= _1761;
        6'b000100:
            _1763 <= vdd;
        6'b000111:
            _1763 <= vdd;
        default:
            _1763 <= _1760;
        endcase
    end
    assign execute_unit_enable = _1763;
    assign _1797 = execute_unit_enable ? _1796 : _1765;
    assign execute_result = _1797;
    always @* begin
        case (instr)
        8'b00001010:
            _1800 <= execute_result;
        8'b00101010:
            _1800 <= execute_result;
        8'b01001010:
            _1800 <= execute_result;
        8'b01101010:
            _1800 <= execute_result;
        8'b10001010:
            _1800 <= regs$x;
        8'b10011000:
            _1800 <= regs$y;
        default:
            _1800 <= _1799;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000011:
            _1801 <= _1800;
        6'b100110:
            _1801 <= _46;
        default:
            _1801 <= _1799;
        endcase
    end
    assign _36 = _1801;
    always @(posedge _44) begin
        if (_42)
            regs$a <= _121;
        else
            if (_40)
                regs$a <= _36;
    end
    always @* begin
        case (instr)
        8'b10001000:
            _1804 <= _903;
        8'b10101000:
            _1804 <= regs$a;
        8'b11001000:
            _1804 <= _923;
        default:
            _1804 <= _1803;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000011:
            _1805 <= _1804;
        default:
            _1805 <= _1803;
        endcase
    end
    assign _37 = _1805;
    always @(posedge _44) begin
        if (_42)
            regs$y <= _121;
        else
            if (_40)
                regs$y <= _37;
    end
    assign _903 = regs$y - _1917;
    assign _1806 = _903[7:7];
    always @* begin
        case (instr)
        8'b10001000:
            _1846 <= _1806;
        8'b10001010:
            _1846 <= _1807;
        8'b10011000:
            _1846 <= _1808;
        8'b10101000:
            _1846 <= _1809;
        8'b10101010:
            _1846 <= _1810;
        8'b10111010:
            _1846 <= _1811;
        8'b11001000:
            _1846 <= _1812;
        8'b11001010:
            _1846 <= _1813;
        8'b11101000:
            _1846 <= _1814;
        default:
            _1846 <= _1845;
        endcase
    end
    always @* begin
        case (state_0)
        6'b000011:
            _1850 <= _1846;
        6'b100000:
            _1850 <= _1847;
        6'b100110:
            _1850 <= _1848;
        6'b101000:
            _1850 <= _1849;
        default:
            _1850 <= _1845;
        endcase
    end
    assign _38 = _1850;
    always @(posedge _44) begin
        if (_42)
            flags$n <= _91;
        else
            if (_40)
                flags$n <= _38;
    end
    assign _274 = flags$n == gnd;
    assign _1855 = _274 ? _352 : _1405;
    assign _88 = 6'b101010;
    assign _40 = enable;
    assign _42 = clear;
    assign _44 = clock;
    assign _46 = data;
    always @* begin
        case (state_0)
        6'b000010:
            _1851 <= _46;
        default:
            _1851 <= instr;
        endcase
    end
    assign _47 = _1851;
    always @(posedge _44) begin
        if (_42)
            instr <= _121;
        else
            if (_40)
                instr <= _47;
    end
    always @* begin
        case (instr)
        8'b00000000:
            _1896 <= _88;
        8'b00000001:
            _1896 <= _1355;
        8'b00000010:
            _1896 <= _1501;
        8'b00000011:
            _1896 <= _1355;
        8'b00001000:
            _1896 <= _1547;
        8'b00001001:
            _1896 <= _1405;
        8'b00001010:
            _1896 <= _1405;
        8'b00001011:
            _1896 <= _1405;
        8'b00001100:
            _1896 <= _1603;
        8'b00001101:
            _1896 <= _1603;
        8'b00001110:
            _1896 <= _1603;
        8'b00001111:
            _1896 <= _1603;
        8'b00010000:
            _1896 <= _1855;
        8'b00010001:
            _1896 <= _339;
        8'b00010010:
            _1896 <= _1501;
        8'b00010011:
            _1896 <= _339;
        8'b00010100:
            _1896 <= _1294;
        8'b00010101:
            _1896 <= _1294;
        8'b00010110:
            _1896 <= _1294;
        8'b00010111:
            _1896 <= _1294;
        8'b00011000:
            _1896 <= _1405;
        8'b00011001:
            _1896 <= _263;
        8'b00011010:
            _1896 <= _1405;
        8'b00011011:
            _1896 <= _263;
        8'b00011100:
            _1896 <= _263;
        8'b00011101:
            _1896 <= _263;
        8'b00011110:
            _1896 <= _263;
        8'b00011111:
            _1896 <= _263;
        8'b00100000:
            _1896 <= _1858;
        8'b00100001:
            _1896 <= _1355;
        8'b00100010:
            _1896 <= _1501;
        8'b00100011:
            _1896 <= _1355;
        8'b00101000:
            _1896 <= _1276;
        8'b00101001:
            _1896 <= _1405;
        8'b00101010:
            _1896 <= _1405;
        8'b00101011:
            _1896 <= _1405;
        8'b00101100:
            _1896 <= _1603;
        8'b00101101:
            _1896 <= _1603;
        8'b00101110:
            _1896 <= _1603;
        8'b00101111:
            _1896 <= _1603;
        8'b00110000:
            _1896 <= _1861;
        8'b00110001:
            _1896 <= _339;
        8'b00110010:
            _1896 <= _1501;
        8'b00110011:
            _1896 <= _339;
        8'b00110100:
            _1896 <= _1294;
        8'b00110101:
            _1896 <= _1294;
        8'b00110110:
            _1896 <= _1294;
        8'b00110111:
            _1896 <= _1294;
        8'b00111000:
            _1896 <= _1405;
        8'b00111001:
            _1896 <= _263;
        8'b00111010:
            _1896 <= _1405;
        8'b00111011:
            _1896 <= _263;
        8'b00111100:
            _1896 <= _263;
        8'b00111101:
            _1896 <= _263;
        8'b00111110:
            _1896 <= _263;
        8'b00111111:
            _1896 <= _263;
        8'b01000000:
            _1896 <= _1262;
        8'b01000001:
            _1896 <= _1355;
        8'b01000010:
            _1896 <= _1501;
        8'b01000011:
            _1896 <= _1355;
        8'b01001000:
            _1896 <= _86;
        8'b01001001:
            _1896 <= _1405;
        8'b01001010:
            _1896 <= _1405;
        8'b01001011:
            _1896 <= _1405;
        8'b01001100:
            _1896 <= _878;
        8'b01001101:
            _1896 <= _1603;
        8'b01001110:
            _1896 <= _1603;
        8'b01001111:
            _1896 <= _1603;
        8'b01010000:
            _1896 <= _1867;
        8'b01010001:
            _1896 <= _339;
        8'b01010010:
            _1896 <= _1501;
        8'b01010011:
            _1896 <= _339;
        8'b01010100:
            _1896 <= _1294;
        8'b01010101:
            _1896 <= _1294;
        8'b01010110:
            _1896 <= _1294;
        8'b01010111:
            _1896 <= _1294;
        8'b01011000:
            _1896 <= _1405;
        8'b01011001:
            _1896 <= _263;
        8'b01011010:
            _1896 <= _1405;
        8'b01011011:
            _1896 <= _263;
        8'b01011100:
            _1896 <= _263;
        8'b01011101:
            _1896 <= _263;
        8'b01011110:
            _1896 <= _263;
        8'b01011111:
            _1896 <= _263;
        8'b01100000:
            _1896 <= _1257;
        8'b01100001:
            _1896 <= _1355;
        8'b01100010:
            _1896 <= _1501;
        8'b01100011:
            _1896 <= _1355;
        8'b01101000:
            _1896 <= _1273;
        8'b01101001:
            _1896 <= _1405;
        8'b01101010:
            _1896 <= _1405;
        8'b01101011:
            _1896 <= _1405;
        8'b01101100:
            _1896 <= _270;
        8'b01101101:
            _1896 <= _1603;
        8'b01101110:
            _1896 <= _1603;
        8'b01101111:
            _1896 <= _1603;
        8'b01110000:
            _1896 <= _1873;
        8'b01110001:
            _1896 <= _339;
        8'b01110010:
            _1896 <= _1501;
        8'b01110011:
            _1896 <= _339;
        8'b01110100:
            _1896 <= _1294;
        8'b01110101:
            _1896 <= _1294;
        8'b01110110:
            _1896 <= _1294;
        8'b01110111:
            _1896 <= _1294;
        8'b01111000:
            _1896 <= _1405;
        8'b01111001:
            _1896 <= _263;
        8'b01111010:
            _1896 <= _1405;
        8'b01111011:
            _1896 <= _263;
        8'b01111100:
            _1896 <= _263;
        8'b01111101:
            _1896 <= _263;
        8'b01111110:
            _1896 <= _263;
        8'b01111111:
            _1896 <= _263;
        8'b10000000:
            _1896 <= _1405;
        8'b10000001:
            _1896 <= _1355;
        8'b10000010:
            _1896 <= _1405;
        8'b10000011:
            _1896 <= _1355;
        8'b10001000:
            _1896 <= _1405;
        8'b10001001:
            _1896 <= _1405;
        8'b10001010:
            _1896 <= _1405;
        8'b10001011:
            _1896 <= _1501;
        8'b10001100:
            _1896 <= _1603;
        8'b10001101:
            _1896 <= _1603;
        8'b10001110:
            _1896 <= _1603;
        8'b10001111:
            _1896 <= _1603;
        8'b10010000:
            _1896 <= _1877;
        8'b10010001:
            _1896 <= _339;
        8'b10010010:
            _1896 <= _1501;
        8'b10010011:
            _1896 <= _339;
        8'b10010100:
            _1896 <= _1294;
        8'b10010101:
            _1896 <= _1294;
        8'b10010110:
            _1896 <= _1294;
        8'b10010111:
            _1896 <= _1294;
        8'b10011000:
            _1896 <= _1405;
        8'b10011001:
            _1896 <= _263;
        8'b10011010:
            _1896 <= _1405;
        8'b10011011:
            _1896 <= _1501;
        8'b10011100:
            _1896 <= _263;
        8'b10011101:
            _1896 <= _263;
        8'b10011110:
            _1896 <= _263;
        8'b10011111:
            _1896 <= _263;
        8'b10100000:
            _1896 <= _1405;
        8'b10100001:
            _1896 <= _1355;
        8'b10100010:
            _1896 <= _1405;
        8'b10100011:
            _1896 <= _1355;
        8'b10101000:
            _1896 <= _1405;
        8'b10101001:
            _1896 <= _1405;
        8'b10101010:
            _1896 <= _1405;
        8'b10101011:
            _1896 <= _1405;
        8'b10101100:
            _1896 <= _1603;
        8'b10101101:
            _1896 <= _1603;
        8'b10101110:
            _1896 <= _1603;
        8'b10101111:
            _1896 <= _1603;
        8'b10110000:
            _1896 <= _1880;
        8'b10110001:
            _1896 <= _339;
        8'b10110010:
            _1896 <= _1501;
        8'b10110011:
            _1896 <= _339;
        8'b10110100:
            _1896 <= _1294;
        8'b10110101:
            _1896 <= _1294;
        8'b10110110:
            _1896 <= _1294;
        8'b10110111:
            _1896 <= _1294;
        8'b10111000:
            _1896 <= _1405;
        8'b10111001:
            _1896 <= _263;
        8'b10111010:
            _1896 <= _1405;
        8'b10111011:
            _1896 <= _1501;
        8'b10111100:
            _1896 <= _263;
        8'b10111101:
            _1896 <= _263;
        8'b10111110:
            _1896 <= _263;
        8'b10111111:
            _1896 <= _263;
        8'b11000000:
            _1896 <= _1405;
        8'b11000001:
            _1896 <= _1355;
        8'b11000010:
            _1896 <= _1405;
        8'b11000011:
            _1896 <= _1355;
        8'b11001000:
            _1896 <= _1405;
        8'b11001001:
            _1896 <= _1405;
        8'b11001010:
            _1896 <= _1405;
        8'b11001011:
            _1896 <= _1405;
        8'b11001100:
            _1896 <= _1603;
        8'b11001101:
            _1896 <= _1603;
        8'b11001110:
            _1896 <= _1603;
        8'b11001111:
            _1896 <= _1603;
        8'b11010000:
            _1896 <= _1883;
        8'b11010001:
            _1896 <= _339;
        8'b11010010:
            _1896 <= _1501;
        8'b11010011:
            _1896 <= _339;
        8'b11010100:
            _1896 <= _1294;
        8'b11010101:
            _1896 <= _1294;
        8'b11010110:
            _1896 <= _1294;
        8'b11010111:
            _1896 <= _1294;
        8'b11011000:
            _1896 <= _1405;
        8'b11011001:
            _1896 <= _263;
        8'b11011010:
            _1896 <= _1405;
        8'b11011011:
            _1896 <= _263;
        8'b11011100:
            _1896 <= _263;
        8'b11011101:
            _1896 <= _263;
        8'b11011110:
            _1896 <= _263;
        8'b11011111:
            _1896 <= _263;
        8'b11100000:
            _1896 <= _1405;
        8'b11100001:
            _1896 <= _1355;
        8'b11100010:
            _1896 <= _1405;
        8'b11100011:
            _1896 <= _1355;
        8'b11101000:
            _1896 <= _1405;
        8'b11101001:
            _1896 <= _1405;
        8'b11101010:
            _1896 <= _1405;
        8'b11101011:
            _1896 <= _1405;
        8'b11101100:
            _1896 <= _1603;
        8'b11101101:
            _1896 <= _1603;
        8'b11101110:
            _1896 <= _1603;
        8'b11101111:
            _1896 <= _1603;
        8'b11110000:
            _1896 <= _1887;
        8'b11110001:
            _1896 <= _339;
        8'b11110010:
            _1896 <= _1501;
        8'b11110011:
            _1896 <= _339;
        8'b11110100:
            _1896 <= _1294;
        8'b11110101:
            _1896 <= _1294;
        8'b11110110:
            _1896 <= _1294;
        8'b11110111:
            _1896 <= _1294;
        8'b11111000:
            _1896 <= _1405;
        8'b11111001:
            _1896 <= _263;
        8'b11111010:
            _1896 <= _1405;
        8'b11111011:
            _1896 <= _263;
        8'b11111100:
            _1896 <= _263;
        8'b11111101:
            _1896 <= _263;
        8'b11111110:
            _1896 <= _263;
        8'b11111111:
            _1896 <= _263;
        default:
            _1896 <= _1895;
        endcase
    end
    assign _1852 = 6'b101001;
    always @* begin
        case (state_0)
        6'b000000:
            _1901 <= _1852;
        6'b000010:
            _1901 <= _1379;
        6'b000011:
            _1901 <= _1896;
        6'b000100:
            _1901 <= _1405;
        6'b000101:
            _1901 <= _1405;
        6'b000110:
            _1901 <= _83;
        6'b000111:
            _1901 <= _82;
        6'b001010:
            _1901 <= _1897;
        6'b001100:
            _1901 <= _336;
        6'b001101:
            _1901 <= _268;
        6'b001111:
            _1901 <= _269;
        6'b010000:
            _1901 <= _1898;
        6'b010001:
            _1901 <= _1900;
        6'b010010:
            _1901 <= _1405;
        6'b010011:
            _1901 <= _1405;
        6'b010100:
            _1901 <= _353;
        6'b010101:
            _1901 <= _882;
        6'b010110:
            _1901 <= _1405;
        6'b010111:
            _1901 <= _84;
        6'b011000:
            _1901 <= _85;
        6'b011001:
            _1901 <= _886;
        6'b011010:
            _1901 <= _1405;
        6'b011011:
            _1901 <= _354;
        6'b011100:
            _1901 <= _888;
        6'b011101:
            _1901 <= _891;
        6'b011110:
            _1901 <= _1405;
        6'b011111:
            _1901 <= _101;
        6'b100000:
            _1901 <= _355;
        6'b100001:
            _1901 <= _893;
        6'b100010:
            _1901 <= _1405;
        6'b100011:
            _1901 <= _1405;
        6'b100100:
            _1901 <= _1405;
        6'b100101:
            _1901 <= _1140;
        6'b100110:
            _1901 <= _1405;
        6'b100111:
            _1901 <= _103;
        6'b101000:
            _1901 <= _1405;
        6'b101001:
            _1901 <= _88;
        6'b101010:
            _1901 <= _89;
        6'b101011:
            _1901 <= _90;
        6'b101100:
            _1901 <= _896;
        6'b101101:
            _1901 <= _900;
        6'b101110:
            _1901 <= _1405;
        default:
            _1901 <= _1895;
        endcase
    end
    assign _48 = _1901;
    always @(posedge _44) begin
        if (_42)
            state_0 <= _52;
        else
            if (_40)
                state_0 <= _48;
    end
    always @* begin
        case (state_0)
        6'b000000:
            _1930 <= pc_0;
        6'b000001:
            _1930 <= _1903;
        6'b000010:
            _1930 <= pc_0;
        6'b000011:
            _1930 <= pc_0;
        6'b000100:
            _1930 <= effective_addr;
        6'b000101:
            _1930 <= effective_addr;
        6'b000110:
            _1930 <= effective_addr;
        6'b000111:
            _1930 <= effective_addr;
        6'b001000:
            _1930 <= pc_0;
        6'b001001:
            _1930 <= effective_addr;
        6'b001010:
            _1930 <= pc_0;
        6'b001011:
            _1930 <= effective_addr;
        6'b001100:
            _1930 <= _1906;
        6'b001101:
            _1930 <= _1908;
        6'b001110:
            _1930 <= _1910;
        6'b001111:
            _1930 <= _1912;
        6'b010000:
            _1930 <= _1914;
        6'b010001:
            _1930 <= pc_0;
        6'b010010:
            _1930 <= _1916;
        6'b010011:
            _1930 <= pc_0;
        6'b010100:
            _1930 <= pc_0;
        6'b010101:
            _1930 <= effective_addr;
        6'b010110:
            _1930 <= effective_addr;
        6'b010111:
            _1930 <= stack_addr;
        6'b011000:
            _1930 <= stack_addr;
        6'b011001:
            _1930 <= stack_addr;
        6'b011010:
            _1930 <= pc_0;
        6'b011011:
            _1930 <= stack_addr;
        6'b011100:
            _1930 <= stack_addr;
        6'b011101:
            _1930 <= stack_addr;
        6'b011111:
            _1930 <= stack_addr;
        6'b100000:
            _1930 <= stack_addr;
        6'b100001:
            _1930 <= stack_addr;
        6'b100010:
            _1930 <= stack_addr;
        6'b100011:
            _1930 <= stack_addr;
        6'b100100:
            _1930 <= stack_addr;
        6'b100101:
            _1930 <= stack_addr;
        6'b100110:
            _1930 <= stack_addr;
        6'b100111:
            _1930 <= stack_addr;
        6'b101000:
            _1930 <= stack_addr;
        6'b101001:
            _1930 <= pc_0;
        6'b101010:
            _1930 <= stack_addr;
        6'b101011:
            _1930 <= stack_addr;
        6'b101100:
            _1930 <= stack_addr;
        6'b101101:
            _1930 <= _1923;
        6'b101110:
            _1930 <= _1928;
        default:
            _1930 <= _1929;
        endcase
    end
    assign mem_port$addr = _1930;
    assign mem$addr = mem_port$addr;
    assign mem$data = mem_port$data;
    assign mem$write = mem_port$write;
    assign state = state_0;
    assign fetching = _80;
    assign pc = pc_0;
    assign a = regs$a;
    assign s = regs$s;
    assign x = regs$x;
    assign y = regs$y;
    assign p = _68;
    assign illegal = _54;

endmodule
module memory (
    enable,
    clock,
    data_in,
    addr,
    write,
    data
);

    input enable;
    input clock;
    input [7:0] data_in;
    input [15:0] addr;
    input write;
    output [7:0] data;

    wire vdd;
    wire gnd;
    wire _17;
    wire [13:0] rom_addr;
    wire [7:0] _20;
    wire [7:0] _1;
    wire _27;
    wire _28;
    reg [7:0] _29[0:2047];
    wire [10:0] ram_addr;
    wire [7:0] _30;
    reg [7:0] _31;
    wire _3;
    wire _5;
    wire [7:0] _7;
    wire [7:0] _21;
    wire [7:0] _8;
    reg [7:0] last_bus;
    wire [2:0] _23;
    wire [2:0] _22;
    wire ram_range;
    wire [7:0] _32;
    wire [15:0] _10;
    wire rom_range;
    wire [7:0] _33;
    wire _12;
    wire [7:0] _34;
    wire [7:0] data_out;
    assign vdd = 1'b1;
    assign gnd = 1'b0;
    assign _17 = _3 & rom_range;
    assign rom_addr = _10[13:0];
    xpm_memory_sprom
        #( .ADDR_WIDTH_A(14),
           .AUTO_SLEEP_TIME(0),
           .CASCADE_HEIGHT(0),
           .ECC_BIT_RANGE("7:0"),
           .ECC_MODE("no_ecc"),
           .ECC_TYPE("none"),
           .IGNORE_INIT_SYNTH(0),
           .MEMORY_INIT_FILE("prg-rom.mem"),
           .MEMORY_INIT_PARAM("0"),
           .MEMORY_OPTIMIZATION("true"),
           .MEMORY_PRIMITIVE("auto"),
           .MEMORY_SIZE(131072),
           .MESSAGE_CONTROL(0),
           .RAM_DECOMP("auto"),
           .READ_DATA_WIDTH_A(8),
           .READ_LATENCY_A(1),
           .READ_RESET_VALUE_A("0"),
           .RST_MODE_A("SYNC"),
           .SIM_ASSERT_CHK(0),
           .USE_MEM_INIT(1),
           .USE_MEM_INIT_MMI(0),
           .WAKEUP_TIME("disable_sleep") )
        the_xpm_memory_sprom
        ( .addra(rom_addr),
          .clka(_5),
          .ena(_17),
          .rsta(gnd),
          .regcea(vdd),
          .sleep(gnd),
          .injectdbiterra(gnd),
          .injectsbiterra(gnd),
          .douta(_20[7:0]) );
    assign _1 = _20;
    assign _27 = _3 & _12;
    assign _28 = _27 & ram_range;
    always @(posedge _5) begin
        if (_28)
            _29[ram_addr] <= _7;
    end
    assign ram_addr = _10[10:0];
    assign _30 = _29[ram_addr];
    always @(posedge _5) begin
        if (_3)
            _31 <= _30;
    end
    assign _3 = enable;
    assign _5 = clock;
    assign _7 = data_in;
    assign _21 = _12 ? _7 : data_out;
    assign _8 = _21;
    always @(posedge _5) begin
        if (_3)
            last_bus <= _8;
    end
    assign _23 = 3'b000;
    assign _22 = _10[15:13];
    assign ram_range = _22 == _23;
    assign _32 = ram_range ? _31 : last_bus;
    assign _10 = addr;
    assign rom_range = _10[15:15];
    assign _33 = rom_range ? _1 : _32;
    assign _12 = write;
    assign _34 = _12 ? last_bus : _33;
    assign data_out = _34;
    assign data = data_out;

endmodule
module nexys_a7_100t (
    usb_uart_rxd,
    reset_n,
    clock_100,
    usb_uart_txd,
    usb_uart_cts
);

    input usb_uart_rxd;
    input reset_n;
    input clock_100;
    output usb_uart_txd;
    output usb_uart_cts;

    wire _222;
    wire _218;
    wire _41;
    wire _45;
    wire _46;
    wire _47;
    reg _49;
    wire _2;
    reg _42;
    reg _220;
    wire _221;
    wire [8:0] _43;
    wire [7:0] _70;
    wire [8:0] _71;
    wire [8:0] _72;
    wire [7:0] _67;
    wire [8:0] _68;
    reg [8:0] _73;
    wire [8:0] _3;
    reg [8:0] _44;
    wire _215;
    wire _216;
    wire _214;
    wire [2:0] _38;
    wire [2:0] _211;
    wire _76;
    wire _77;
    reg _79;
    wire _4;
    reg _75;
    wire [2:0] _207;
    wire [2:0] _197;
    reg [2:0] _209;
    wire [2:0] _210;
    wire [2:0] _78;
    wire [2:0] _205;
    wire [2:0] _202;
    wire [3:0] _200;
    wire [3:0] _81;
    wire [3:0] _83;
    wire [3:0] _84;
    wire [3:0] _85;
    reg [3:0] _86;
    wire [3:0] _5;
    reg [3:0] data_count;
    wire _201;
    wire [2:0] _203;
    wire [2:0] _204;
    wire [2:0] _48;
    wire _173;
    reg _174;
    wire _175;
    reg _176;
    wire _171;
    wire _169;
    wire _170;
    wire _167;
    wire _163;
    wire [15:0] _146;
    wire [15:0] _144;
    wire [15:0] _142;
    wire _158;
    wire [15:0] _160;
    wire _135;
    wire _131;
    wire _132;
    wire _92;
    reg _94;
    wire _95;
    wire _6;
    reg parity;
    wire _128;
    wire _129;
    wire [1:0] _127;
    reg _134;
    wire [1:0] _89;
    wire _99;
    reg _101;
    wire _102;
    wire _7;
    reg _97;
    wire [1:0] _121;
    reg [1:0] _123;
    wire _117;
    wire _118;
    wire [1:0] _124;
    wire [1:0] _100;
    wire [1:0] _114;
    wire [3:0] _107;
    reg [3:0] _108;
    wire [3:0] _109;
    wire [3:0] _8;
    reg [3:0] data_count_1;
    wire _113;
    wire [1:0] _115;
    wire _110;
    wire [1:0] _111;
    reg [1:0] _125;
    wire [1:0] _126;
    wire [1:0] _9;
    (* fsm_encoding="one_hot" *)
    reg [1:0] sm_rx;
    reg _136;
    wire _137;
    wire _10;
    reg _153;
    wire _156;
    wire [15:0] _162;
    wire [15:0] _149;
    wire [15:0] _150;
    reg [15:0] _164;
    wire [15:0] _11;
    reg [15:0] enable_count;
    wire [15:0] _145;
    wire _147;
    wire _165;
    wire _140;
    wire _166;
    reg _168;
    wire _13;
    (* fsm_encoding="one_hot" *)
    reg sm_top;
    reg _172;
    wire enable;
    wire _177;
    wire _15;
    reg _155;
    wire _16;
    wire _65;
    wire [7:0] _64;
    wire [7:0] _63;
    wire [7:0] _62;
    wire [7:0] _61;
    wire [7:0] _60;
    wire [15:0] _59;
    wire _58;
    wire [5:0] _57;
    wire _56;
    wire write;
    wire [7:0] _55;
    wire [7:0] write_data;
    wire [15:0] addr;
    wire _178;
    wire _179;
    wire [7:0] _180;
    wire [7:0] _20;
    wire _181;
    wire _21;
    reg _51;
    wire _182;
    wire cpu_enable;
    wire _52;
    wire [88:0] _53;
    wire [15:0] _54;
    wire vdd;
    wire [15:0] _157;
    wire [15:0] _190;
    reg [15:0] _191;
    wire [15:0] _23;
    reg [15:0] enable_rate;
    wire [15:0] _187;
    wire _188;
    wire _192;
    wire enable_1;
    wire _196;
    wire gnd;
    wire _195;
    reg _198;
    wire _25;
    reg _194;
    wire _26;
    wire _36;
    wire _37;
    wire _33;
    wire [1:0] _34;
    wire _35;
    wire [9:0] _66;
    wire _193;
    wire [2:0] _199;
    reg [2:0] _212;
    wire [2:0] _29;
    (* fsm_encoding="one_hot" *)
    reg [2:0] sm_tx;
    reg _223;
    wire _30;
    reg _213;
    assign _222 = enable_1 ? vdd : _213;
    assign _218 = ~ _42;
    assign _41 = 1'b0;
    assign _45 = _44[0:0];
    assign _46 = _42 ^ _45;
    assign _47 = enable_1 ? _46 : _42;
    always @* begin
        case (sm_tx)
        3'b000:
            _49 <= gnd;
        3'b001:
            _49 <= _47;
        default:
            _49 <= _42;
        endcase
    end
    assign _2 = _49;
    always @(posedge _35) begin
        if (_37)
            _42 <= _41;
        else
            _42 <= _2;
    end
    always @* begin
        case (_127)
        2'b01:
            _220 <= _42;
        2'b10:
            _220 <= _218;
        default:
            _220 <= _213;
        endcase
    end
    assign _221 = enable_1 ? _220 : _213;
    assign _43 = 9'b000000000;
    assign _70 = _44[8:1];
    assign _71 = { _41,
                   _70 };
    assign _72 = enable_1 ? _71 : _44;
    assign _67 = _66[8:1];
    assign _68 = { gnd,
                   _67 };
    always @* begin
        case (sm_tx)
        3'b000:
            _73 <= _68;
        3'b001:
            _73 <= _72;
        default:
            _73 <= _44;
        endcase
    end
    assign _3 = _73;
    always @(posedge _35) begin
        if (_37)
            _44 <= _43;
        else
            _44 <= _3;
    end
    assign _215 = _44[0:0];
    assign _216 = enable_1 ? _215 : _213;
    assign _214 = _193 ? gnd : _213;
    assign _38 = 3'b000;
    assign _211 = enable_1 ? _38 : sm_tx;
    assign _76 = ~ _75;
    assign _77 = enable_1 ? _76 : _75;
    always @* begin
        case (sm_tx)
        3'b000:
            _79 <= gnd;
        3'b011:
            _79 <= _77;
        default:
            _79 <= _75;
        endcase
    end
    assign _4 = _79;
    always @(posedge _35) begin
        if (_37)
            _75 <= _41;
        else
            _75 <= _4;
    end
    assign _207 = _75 ? _197 : sm_tx;
    assign _197 = 3'b100;
    always @* begin
        case (_41)
        1'b0:
            _209 <= _197;
        1'b1:
            _209 <= _207;
        default:
            _209 <= sm_tx;
        endcase
    end
    assign _210 = enable_1 ? _209 : sm_tx;
    assign _78 = 3'b011;
    assign _205 = enable_1 ? _78 : sm_tx;
    assign _202 = 3'b010;
    assign _200 = 4'b0111;
    assign _81 = 4'b0000;
    assign _83 = 4'b0001;
    assign _84 = data_count + _83;
    assign _85 = enable_1 ? _84 : data_count;
    always @* begin
        case (sm_tx)
        3'b000:
            _86 <= _81;
        3'b001:
            _86 <= _85;
        default:
            _86 <= data_count;
        endcase
    end
    assign _5 = _86;
    always @(posedge _35) begin
        if (_37)
            data_count <= _81;
        else
            data_count <= _5;
    end
    assign _201 = data_count == _200;
    assign _203 = _201 ? _202 : sm_tx;
    assign _204 = enable_1 ? _203 : sm_tx;
    assign _48 = 3'b001;
    assign _173 = _97 ? vdd : gnd;
    always @* begin
        case (_41)
        1'b0:
            _174 <= vdd;
        1'b1:
            _174 <= _173;
        default:
            _174 <= gnd;
        endcase
    end
    assign _175 = _118 ? gnd : _174;
    always @* begin
        case (sm_rx)
        2'b11:
            _176 <= _175;
        default:
            _176 <= gnd;
        endcase
    end
    assign _171 = _158 ? vdd : gnd;
    assign _169 = _147 ? vdd : gnd;
    assign _170 = _140 ? _169 : gnd;
    assign _167 = _156 ? _41 : sm_top;
    assign _163 = 1'b1;
    assign _146 = 16'b0000000001011101;
    assign _144 = 16'b0000000000000001;
    assign _142 = 16'b0000000000000000;
    assign _158 = _145 == _157;
    assign _160 = _158 ? _142 : _145;
    assign _135 = _118 ? vdd : gnd;
    assign _131 = usb_uart_rxd == parity;
    assign _132 = _131 ? vdd : gnd;
    assign _92 = parity ^ usb_uart_rxd;
    always @* begin
        case (sm_rx)
        2'b00:
            _94 <= gnd;
        2'b01:
            _94 <= _92;
        default:
            _94 <= parity;
        endcase
    end
    assign _95 = enable ? _94 : parity;
    assign _6 = _95;
    always @(posedge _35) begin
        if (_37)
            parity <= _41;
        else
            parity <= _6;
    end
    assign _128 = usb_uart_rxd == parity;
    assign _129 = _128 ? gnd : vdd;
    assign _127 = 2'b01;
    always @* begin
        case (_127)
        2'b01:
            _134 <= _129;
        2'b10:
            _134 <= _132;
        default:
            _134 <= gnd;
        endcase
    end
    assign _89 = 2'b00;
    assign _99 = ~ _97;
    always @* begin
        case (sm_rx)
        2'b00:
            _101 <= _41;
        2'b11:
            _101 <= _99;
        default:
            _101 <= _97;
        endcase
    end
    assign _102 = enable ? _101 : _97;
    assign _7 = _102;
    always @(posedge _35) begin
        if (_37)
            _97 <= _41;
        else
            _97 <= _7;
    end
    assign _121 = _97 ? _89 : sm_rx;
    always @* begin
        case (_41)
        1'b0:
            _123 <= _89;
        1'b1:
            _123 <= _121;
        default:
            _123 <= sm_rx;
        endcase
    end
    assign _117 = usb_uart_rxd == vdd;
    assign _118 = ~ _117;
    assign _124 = _118 ? _89 : _123;
    assign _100 = 2'b11;
    assign _114 = 2'b10;
    assign _107 = data_count_1 + _83;
    always @* begin
        case (sm_rx)
        2'b00:
            _108 <= _81;
        2'b01:
            _108 <= _107;
        default:
            _108 <= data_count_1;
        endcase
    end
    assign _109 = enable ? _108 : data_count_1;
    assign _8 = _109;
    always @(posedge _35) begin
        if (_37)
            data_count_1 <= _81;
        else
            data_count_1 <= _8;
    end
    assign _113 = data_count_1 == _200;
    assign _115 = _113 ? _114 : sm_rx;
    assign _110 = ~ usb_uart_rxd;
    assign _111 = _110 ? _127 : sm_rx;
    always @* begin
        case (sm_rx)
        2'b00:
            _125 <= _111;
        2'b01:
            _125 <= _115;
        2'b10:
            _125 <= _100;
        2'b11:
            _125 <= _124;
        default:
            _125 <= sm_rx;
        endcase
    end
    assign _126 = enable ? _125 : sm_rx;
    assign _9 = _126;
    always @(posedge _35) begin
        if (_37)
            sm_rx <= _89;
        else
            sm_rx <= _9;
    end
    always @* begin
        case (sm_rx)
        2'b00:
            _136 <= gnd;
        2'b10:
            _136 <= _134;
        2'b11:
            _136 <= _135;
        default:
            _136 <= gnd;
        endcase
    end
    assign _137 = enable ? _136 : gnd;
    assign _10 = _137;
    always @(posedge _35) begin
        if (_37)
            _153 <= _41;
        else
            _153 <= _10;
    end
    assign _156 = _153 | _155;
    assign _162 = _156 ? _142 : _160;
    assign _149 = _147 ? _142 : _145;
    assign _150 = _140 ? _149 : _142;
    always @* begin
        case (sm_top)
        1'b0:
            _164 <= _150;
        1'b1:
            _164 <= _162;
        default:
            _164 <= enable_count;
        endcase
    end
    assign _11 = _164;
    always @(posedge _35) begin
        if (_37)
            enable_count <= _142;
        else
            enable_count <= _11;
    end
    assign _145 = enable_count + _144;
    assign _147 = _145 == _146;
    assign _165 = _147 ? _163 : sm_top;
    assign _140 = ~ usb_uart_rxd;
    assign _166 = _140 ? _165 : sm_top;
    always @* begin
        case (sm_top)
        1'b0:
            _168 <= _166;
        1'b1:
            _168 <= _167;
        default:
            _168 <= sm_top;
        endcase
    end
    assign _13 = _168;
    always @(posedge _35) begin
        if (_37)
            sm_top <= _41;
        else
            sm_top <= _13;
    end
    always @* begin
        case (sm_top)
        1'b0:
            _172 <= _170;
        1'b1:
            _172 <= _171;
        default:
            _172 <= gnd;
        endcase
    end
    assign enable = _172;
    assign _177 = enable ? _176 : gnd;
    assign _15 = _177;
    always @(posedge _35) begin
        if (_37)
            _155 <= _41;
        else
            _155 <= _15;
    end
    assign _16 = _155;
    assign _65 = _53[88:88];
    assign _64 = _53[87:80];
    assign _63 = _53[79:72];
    assign _62 = _53[71:64];
    assign _61 = _53[63:56];
    assign _60 = _53[55:48];
    assign _59 = _53[47:32];
    assign _58 = _53[31:31];
    assign _57 = _53[30:25];
    assign _56 = _53[24:24];
    assign write = _56;
    assign _55 = _53[23:16];
    assign write_data = _55;
    assign addr = _54;
    assign _178 = ~ _51;
    assign _179 = cpu_enable & _178;
    memory
        memory
        ( .enable(_179),
          .addr(addr),
          .data_in(write_data),
          .write(write),
          .clock(_35),
          .data(_180[7:0]) );
    assign _20 = _180;
    assign _181 = ~ _51;
    assign _21 = _181;
    always @(posedge _35) begin
        if (_37)
            _51 <= _41;
        else
            if (cpu_enable)
                _51 <= _21;
    end
    assign _182 = _66[0:0];
    assign cpu_enable = _182;
    assign _52 = cpu_enable & _51;
    cpu
        cpu
        ( .enable(_52),
          .irq(gnd),
          .nmi(gnd),
          .data(_20),
          .clock(_35),
          .clear(_37),
          .mem$addr(_53[15:0]),
          .mem$data(_53[23:16]),
          .mem$write(_53[24:24]),
          .state(_53[30:25]),
          .fetching(_53[31:31]),
          .pc(_53[47:32]),
          .a(_53[55:48]),
          .s(_53[63:56]),
          .x(_53[71:64]),
          .y(_53[79:72]),
          .p(_53[87:80]),
          .illegal(_53[88:88]) );
    assign _54 = _53[15:0];
    assign vdd = 1'b1;
    assign _157 = 16'b0000000010111010;
    assign _190 = _188 ? _142 : _187;
    always @* begin
        case (sm_tx)
        3'b000:
            _191 <= _142;
        default:
            _191 <= _190;
        endcase
    end
    assign _23 = _191;
    always @(posedge _35) begin
        if (_37)
            enable_rate <= _142;
        else
            enable_rate <= _23;
    end
    assign _187 = enable_rate + _144;
    assign _188 = _187 == _157;
    assign _192 = _188 ? vdd : gnd;
    assign enable_1 = _192;
    assign _196 = enable_1 ? vdd : _194;
    assign gnd = 1'b0;
    assign _195 = _193 ? gnd : _194;
    always @* begin
        case (sm_tx)
        3'b000:
            _198 <= _195;
        3'b100:
            _198 <= _196;
        default:
            _198 <= _194;
        endcase
    end
    assign _25 = _198;
    always @(posedge _35) begin
        if (_37)
            _194 <= vdd;
        else
            _194 <= _25;
    end
    assign _26 = _194;
    assign _36 = _34[1:1];
    assign _37 = ~ _36;
    assign _33 = ~ reset_n;
    clk_wiz
        the_clk_wiz
        ( .clk_in1(clock_100),
          .reset(_33),
          .clk_out1(_34[0:0]),
          .locked(_34[1:1]) );
    assign _35 = _34[0:0];
    tracer
        tracer
        ( .clock(_35),
          .clear(_37),
          .ready(_26),
          .cpu$mem$addr(_54),
          .cpu$mem$data(_55),
          .cpu$mem$write(_56),
          .cpu$state(_57),
          .cpu$fetching(_58),
          .cpu$pc(_59),
          .cpu$a(_60),
          .cpu$s(_61),
          .cpu$x(_62),
          .cpu$y(_63),
          .cpu$p(_64),
          .cpu$illegal(_65),
          .rx_start(_16),
          .cpu_enable(_66[0:0]),
          .tx(_66[8:1]),
          .tx_start(_66[9:9]) );
    assign _193 = _66[9:9];
    assign _199 = _193 ? _48 : sm_tx;
    always @* begin
        case (sm_tx)
        3'b000:
            _212 <= _199;
        3'b001:
            _212 <= _204;
        3'b010:
            _212 <= _205;
        3'b011:
            _212 <= _210;
        3'b100:
            _212 <= _211;
        default:
            _212 <= sm_tx;
        endcase
    end
    assign _29 = _212;
    always @(posedge _35) begin
        if (_37)
            sm_tx <= _38;
        else
            sm_tx <= _29;
    end
    always @* begin
        case (sm_tx)
        3'b000:
            _223 <= _214;
        3'b001:
            _223 <= _216;
        3'b010:
            _223 <= _221;
        3'b011:
            _223 <= _222;
        default:
            _223 <= _213;
        endcase
    end
    assign _30 = _223;
    always @(posedge _35) begin
        if (_37)
            _213 <= vdd;
        else
            _213 <= _30;
    end
    assign usb_uart_txd = _213;
    assign usb_uart_cts = gnd;

endmodule
module nexys_a7_100t_top
(
  input usb_uart_rxd,
  input usb_uart_rts,
  input clock_100,
  input reset_n,
  output usb_uart_txd,
  output usb_uart_cts
);

  nexys_a7_100t _7
  (
    .usb_uart_rxd(usb_uart_rxd), 
    .clock_100(clock_100), 
    .reset_n(reset_n), 
    .usb_uart_txd(usb_uart_txd), 
    .usb_uart_cts(usb_uart_cts)
  );
endmodule
