type t = Bus of Bus.out | Instr of Instr.out | End of { tstates : int; first_t1 : int option }

module J = Hwfrp_sinks.Json

let render = J.to_string
let hex w = function None -> J.Null | Some v -> J.Str (Printf.sprintf "0x%0*x" w v)
let hexs w = function None -> "~" | Some v -> Printf.sprintf "%0*x" w v
let width_of = function "sp" | "pc" -> 4 | _ -> 2

let regs_json = function
  | None -> J.Null
  | Some r -> J.Obj (List.map (fun n -> (n, hex (width_of n) (Regs.get r n))) Regs.names)

let cycle_fields (c : Bus.cycle) =
  [
    ("epoch", J.Int c.epoch);
    ("seq", J.Int c.seq);
    ("kind", J.Str (Bus.kind_name c.kind));
    ("status", hex 2 c.status);
    ("addr", hex 4 c.addr);
    ("data", hex 2 c.data);
    ("first", J.Int c.first);
    ("last", J.Int c.last);
    ("waits", J.Int c.waits);
  ]

let changes_json (i : Instr.t) =
  match (i.before, i.after) with
  | Some b, Some a ->
      J.List
        (List.map
           (fun (n, x, y) ->
             J.Obj [ ("reg", J.Str n); ("from", hex (width_of n) x); ("to", hex (width_of n) y) ])
           (Regs.changes b a))
  | _ -> J.Null

let bytes_text bytes = String.concat " " (List.map (Printf.sprintf "%02x") bytes)

let header (m : Manifest.t) =
  render
    (J.Obj
       [
         ("format", J.Str "i8080-events/1"); ("manifest", J.Str m.id); ("profile", J.Str m.profile);
       ])

let to_json = function
  | End { tstates; first_t1 } ->
      Some
        (render
           (J.Obj
              [
                ("ev", J.Str "end");
                ("tstates", J.Int tstates);
                ("first_t1", match first_t1 with Some n -> J.Int n | None -> J.Null);
              ]))
  | Bus (Bus.Start _) -> None
  | Bus (Bus.Reset { epoch; at }) ->
      Some (render (J.Obj [ ("ev", J.Str "reset"); ("epoch", J.Int epoch); ("at", J.Int at) ]))
  | Bus (Bus.Done c) -> Some (render (J.Obj (("ev", J.Str "transfer") :: cycle_fields c)))
  | Bus (Bus.Abandoned { cycle; why }) ->
      Some
        (render
           (J.Obj ((("ev", J.Str "abandoned") :: cycle_fields cycle) @ [ ("why", J.Str why) ])))
  | Bus (Bus.Note { at; msg }) | Instr (Instr.Note { at; msg }) ->
      Some (render (J.Obj [ ("ev", J.Str "note"); ("at", J.Int at); ("msg", J.Str msg) ]))
  | Instr (Instr.Halt_evidence { first; samples }) ->
      Some
        (render
           (J.Obj
              [
                ("ev", J.Str "halt_evidence");
                ("first", J.Int first);
                ( "samples",
                  J.List
                    (List.map
                       (fun (n, a, pc) ->
                         J.Obj [ ("n", J.Int n); ("a", hex 4 a); ("pc", hex 4 pc) ])
                       samples) );
              ]))
  | Instr (Instr.Instruction i) ->
      Some
        (render
           (J.Obj
              [
                ("ev", J.Str "instr");
                ("idx", J.Int i.idx);
                ("epoch", J.Int i.epoch);
                ("pc", hex 4 i.pc);
                ("bytes", J.Str (bytes_text i.bytes));
                ("text", match i.text with Some t -> J.Str t | None -> J.Null);
                ("first", J.Int i.first);
                ("last", J.Int i.last);
                ("tstates", J.Int i.tstates);
                ("transfers", J.List (List.map (fun (c : Bus.cycle) -> J.Int c.seq) i.accesses));
                ("before", regs_json i.before);
                ("after", regs_json i.after);
                ("changes", changes_json i);
                ("complete", J.Bool i.complete);
                ("why", if i.complete then J.Null else J.Str i.why);
              ]))

let to_text = function
  | End { tstates; first_t1 } ->
      Some
        (Printf.sprintf "end tstates=%d first_t1=%s" tstates
           (match first_t1 with Some n -> string_of_int n | None -> "none"))
  | Bus (Bus.Start _) -> None
  | Bus (Bus.Reset { epoch; at }) -> Some (Printf.sprintf "reset epoch=%d at=%d" epoch at)
  | Bus (Bus.Done c) ->
      Some
        (Printf.sprintf "xfer %-11s a=%s d=%s T%d-%d waits=%d" (Bus.kind_name c.kind)
           (hexs 4 c.addr) (hexs 2 c.data) c.first c.last c.waits)
  | Bus (Bus.Abandoned { cycle = c; why }) ->
      Some
        (Printf.sprintf "abandoned %-11s a=%s d=%s T%d-%d waits=%d: %s" (Bus.kind_name c.kind)
           (hexs 4 c.addr) (hexs 2 c.data) c.first c.last c.waits why)
  | Bus (Bus.Note { at; msg }) | Instr (Instr.Note { at; msg }) ->
      Some (Printf.sprintf "note at=%d %s" at msg)
  | Instr (Instr.Halt_evidence { first; samples }) ->
      Some
        (Printf.sprintf "halt-evidence first=%d %s" first
           (String.concat " "
              (List.map
                 (fun (n, a, pc) -> Printf.sprintf "n=%d:a=%s,pc=%s" n (hexs 4 a) (hexs 4 pc))
                 samples)))
  | Instr (Instr.Instruction i) ->
      let regs =
        match (i.before, i.after) with
        | Some b, Some a ->
            String.concat " "
              (List.map
                 (fun (n, x, y) ->
                   Printf.sprintf "%s:%s->%s" n (hexs (width_of n) x) (hexs (width_of n) y))
                 (Regs.changes b a))
        | _ -> "regs=unobserved"
      in
      Some
        (Printf.sprintf "instr #%d %s: %s [%s] T%d-%d %dT %s%s" i.idx (hexs 4 i.pc)
           (Option.value i.text ~default:"?")
           (bytes_text i.bytes) i.first i.last i.tstates regs
           (if i.complete then "" else " INCOMPLETE: " ^ i.why))

let is_instruction = function Instr (Instr.Instruction i) -> Some i | _ -> None
let transfers l = List.filter_map (function Bus (Bus.Done c) -> Some c | _ -> None) l
let instructions l = List.filter_map is_instruction l
