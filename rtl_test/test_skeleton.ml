open Hardcaml
module Sim = Cyclesim.With_interface (I8080_rtl.Skeleton.I) (I8080_rtl.Skeleton.O)

let%expect_test "the counter counts while enabled" =
  let sim = Sim.create I8080_rtl.Skeleton.create in
  let i = Cyclesim.inputs sim and o = Cyclesim.outputs sim in
  let counts =
    List.map
      (fun en ->
        i.enable := if en then Bits.vdd else Bits.gnd;
        Cyclesim.cycle sim;
        Bits.to_int !(o.count))
      [ true; true; false; true ]
  in
  print_endline (String.concat " " (List.map string_of_int counts));
  [%expect {| 1 2 2 3 |}]
