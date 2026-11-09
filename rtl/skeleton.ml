(* P1: the Hardcaml project builds and simulates. Replaced by the CPU in P2. *)
open Hardcaml

module I = struct
  type 'a t = { clock : 'a; enable : 'a } [@@deriving hardcaml]
end

module O = struct
  type 'a t = { count : 'a [@bits 4] } [@@deriving hardcaml]
end

let create (i : _ I.t) =
  let spec = Reg_spec.create ~clock:i.clock () in
  let count = Signal.reg_fb spec ~enable:i.enable ~width:4 ~f:(fun d -> Signal.(d +:. 1)) in
  { O.count }
