(* A Verilog bench around the emitted [i8080_core] for long runs, where Cyclesim is too slow: 64 KiB of
   RAM, the status latch, the K5 shim behind the release's boot preamble, the console on port 01, and a
   stop at OUT FF or HLT. It does what [Harness.run] does for [i8080_rtl_run com] and counts T-states
   the same way, so the two can be compared on a short program. READY is high, HOLD and INT are low.
   With EXTERNAL_CLOCK defined the clock is a port (a C++ main toggles it, which Verilator runs several
   times faster than a delay); without, the bench clocks itself. *)

let shim =
  let m = Harness.new_memory () in
  Harness.load_shim m;
  List.filter_map
    (fun a -> if Bytes.get m a = '\000' then None else Some (a, Char.code (Bytes.get m a)))
    (List.init 0x10000 Fun.id)

let verilog =
  let b = Buffer.create 4096 in
  let p fmt = Printf.bprintf b fmt in
  let boot = Harness.com_boot in
  let n = String.length boot.code in
  p "`ifdef EXTERNAL_CLOCK\nmodule i8080_bench (input clock);\n`else\nmodule i8080_bench;\n";
  p "  reg clock = 0;\n  always #1 clock = ~clock;\n`endif\n";
  p "  reg [2:0] held = 0;\n  wire rst_in = held != 4;\n";
  p "  wire [15:0] a;\n  wire [7:0] dout;\n  wire sync, dbin, wr_n, wait_;\n";
  p "  reg [7:0] mem [0:65535];\n  reg [7:0] saved [0:%d];\n" (n - 1);
  p "  reg [7:0] status = 0;\n  reg booting = 1, started = 0;\n";
  p "  reg [63:0] n = 0, max_tstates = 0;\n  reg [1023:0] file;\n  integer fd, len, k;\n";
  p "  // an IN reads ff; everything else that is read comes from memory\n";
  p "  wire [7:0] di = status == 8'h42 ? 8'hff : mem[a];\n";
  p "  /* verilator lint_off PINMISSING */\n";
  p "  i8080_core dut (.clock(clock), .tick(1'b1), .rst_in(rst_in), .ready(1'b1), .hold(1'b0),\n";
  p "    .int_req(1'b0), .di(di), .a(a), .dout(dout), .sync(sync), .dbin(dbin), .wr_n(wr_n),\n";
  p "    .wait_(wait_));\n";
  p "  /* verilator lint_on PINMISSING */\n";
  p "  task finish(input [8*8-1:0] why);\n    begin\n";
  p
    "      $display(\"\\nstop %%0s, tstates %%0d\", why, n + 1);\n\
    \      $finish;\n\
    \    end\n\
    \  endtask\n";
  p "  initial begin\n";
  p "    for (k = 0; k < 65536; k = k + 1) mem[k] = 0;\n";
  List.iter (fun (a, v) -> p "    mem[16'h%04x] = 8'h%02x;\n" a v) shim;
  p
    "    if (!$value$plusargs(\"com=%%s\", file)) begin $display(\"bench: +com=<file> is \
     needed\"); $finish; end\n";
  p "    fd = $fopen(file, \"rb\");\n";
  p "    if (fd == 0) begin $display(\"bench: cannot open the program\"); $finish; end\n";
  p "    len = $fread(mem, fd, 16'h0100);\n    $fclose(fd);\n";
  p "    if (!$value$plusargs(\"max_tstates=%%d\", max_tstates)) max_tstates = 0;\n";
  p "    // the boot preamble runs over the shim's first bytes until the first fetch at %04x\n"
    boot.pc;
  p "    for (k = 0; k < %d; k = k + 1) saved[k] = mem[k];\n" n;
  String.iteri (fun i c -> p "    mem[%d] = 8'h%02x;\n" i (Char.code c)) boot.code;
  p "  end\n";
  p "  always @(posedge clock) begin\n";
  p "    if (rst_in) held <= held + 1;\n";
  p "    if (sync) begin\n      status <= dout;\n";
  p "      if (booting && dout == 8'ha2 && a == 16'h%04x) begin\n" boot.pc;
  p "        for (k = 0; k < %d; k = k + 1) mem[k] = saved[k];\n" n;
  p "        booting <= 0;\n        started <= 1;\n        n <= 1;\n      end\n    end\n";
  p "    if (started && !(sync && booting)) n <= n + 1;\n";
  p "    if (!wr_n) begin\n";
  p "      if (status == 8'h10) begin\n";
  p "        if (a[7:0] == 8'h01) $write(\"%%c\", dout);\n";
  p "        if (a[7:0] == 8'hff) finish(\"port ff\");\n";
  p "      end else mem[a] <= dout;\n    end\n";
  p "    if (status == 8'h8a && wait_) finish(\"hlt\");\n";
  p "    if (max_tstates != 0 && n >= max_tstates) finish(\"budget\");\n";
  p "  end\nendmodule\n";
  Buffer.contents b
