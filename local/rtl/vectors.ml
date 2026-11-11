(* Test vectors for the emitted Verilog: one line per clock cycle with the inputs applied and the outputs
   Cyclesim showed before the edge, and a testbench that replays the inputs into [i8080_core] and
   compares. The interfaces are walked, so a port added to [Core] is covered without a change here. *)
open Hardcaml

let all_ins = Core.I.to_list Core.I.port_names_and_widths
let ins = List.filter (fun (n, _) -> n <> "clock") all_ins
let outs = Core.O.to_list Core.O.port_names_and_widths
let width l = List.fold_left (fun n (_, w) -> n + w) 0 l

let line (i : Bits.t ref Core.I.t) (o : Bits.t ref Core.O.t) =
  let ins =
    List.filter_map
      (fun ((n, _), r) -> if n = "clock" then None else Some !r)
      (List.combine all_ins (Core.I.to_list i))
  in
  Bits.to_bstr (Bits.concat_msb (ins @ List.map (fun r -> !r) (Core.O.to_list o)))

(* The first cycle is not compared: the registers hold nothing until the first edge clears them. *)
let testbench =
  let wi = width ins and wo = width outs in
  let b = Buffer.create 2048 in
  let p fmt = Printf.bprintf b fmt in
  let range w = if w = 1 then "" else Printf.sprintf "[%d:0] " (w - 1) in
  let names l = String.concat ", " (List.map fst l) in
  p "module i8080_core_tb;\n";
  p "  reg clock = 0;\n  reg [%d:0] v;\n" (wi + wo - 1);
  List.iter (fun (n, w) -> p "  wire %s%s;\n" (range w) n) (ins @ outs);
  p "  assign {%s} = v[%d:%d];\n" (names ins) (wi + wo - 1) wo;
  p "  wire [%d:0] got = {%s};\n" (wo - 1) (names outs);
  p "  i8080_core dut (.clock(clock)";
  List.iter (fun (n, _) -> p ", .%s(%s)" n n) (ins @ outs);
  p ");\n";
  p "  always #5 clock = ~clock;\n";
  p "  integer fd, n, bad;\n  reg [1023:0] file;\n";
  p "  initial begin\n";
  p
    "    if (!$value$plusargs(\"vectors=%%s\", file)) begin $display(\"tb: +vectors=<file> is \
     needed\"); $finish; end\n";
  p "    fd = $fopen(file, \"r\");\n";
  p "    if (fd == 0) begin $display(\"tb: cannot open the vectors\"); $finish; end\n";
  p "    n = 0; bad = 0;\n";
  p "    while ($fscanf(fd, \"%%b\", v) == 1) begin\n";
  p "      #4;\n";
  p "      if (n > 0 && got !== v[%d:0]) begin\n" (wo - 1);
  p "        bad = bad + 1;\n";
  p "        if (bad <= 10) $display(\"tb: cycle %%0d: got %%h, expected %%h\", n, got, v[%d:0]);\n"
    (wo - 1);
  p "      end\n";
  p "      #6;\n      n = n + 1;\n    end\n";
  p "    $display(\"tb: %%0d cycles, %%0d mismatches\", n, bad);\n    $finish;\n  end\nendmodule\n";
  Buffer.contents b
