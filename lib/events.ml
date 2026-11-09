type t = Bus of Bus.out | Instr of Instr.out | End of { tstates : int; first_t1 : int option }

(* A small JSON writer: fixed key order, no whitespace. *)
type j = I of int | S of string | B of bool | N | A of j list | O of (string * j) list

let rec put b = function
  | I n -> Buffer.add_string b (string_of_int n)
  | S s ->
      Buffer.add_char b '"';
      String.iter
        (function
          | '"' -> Buffer.add_string b "\\\""
          | '\\' -> Buffer.add_string b "\\\\"
          | '\n' -> Buffer.add_string b "\\n"
          | c when Char.code c < 0x20 ->
              Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
          | c -> Buffer.add_char b c)
        s;
      Buffer.add_char b '"'
  | B x -> Buffer.add_string b (if x then "true" else "false")
  | N -> Buffer.add_string b "null"
  | A l ->
      Buffer.add_char b '[';
      List.iteri
        (fun i x ->
          if i > 0 then Buffer.add_char b ',';
          put b x)
        l;
      Buffer.add_char b ']'
  | O l ->
      Buffer.add_char b '{';
      List.iteri
        (fun i (k, x) ->
          if i > 0 then Buffer.add_char b ',';
          put b (S k);
          Buffer.add_char b ':';
          put b x)
        l;
      Buffer.add_char b '}'

let render j =
  let b = Buffer.create 128 in
  put b j;
  Buffer.contents b

let hex w = function None -> N | Some v -> S (Printf.sprintf "0x%0*x" w v)
let hexs w = function None -> "~" | Some v -> Printf.sprintf "%0*x" w v
let width_of = function "sp" | "pc" -> 4 | _ -> 2

let regs_json = function
  | None -> N
  | Some r -> O (List.map (fun n -> (n, hex (width_of n) (Regs.get r n))) Regs.names)

let cycle_fields (c : Bus.cycle) =
  [
    ("epoch", I c.epoch);
    ("seq", I c.seq);
    ("kind", S (Bus.kind_name c.kind));
    ("status", hex 2 c.status);
    ("addr", hex 4 c.addr);
    ("data", hex 2 c.data);
    ("first", I c.first);
    ("last", I c.last);
    ("waits", I c.waits);
  ]

let changes_json (i : Instr.t) =
  match (i.before, i.after) with
  | Some b, Some a ->
      A
        (List.map
           (fun (n, x, y) ->
             O [ ("reg", S n); ("from", hex (width_of n) x); ("to", hex (width_of n) y) ])
           (Regs.changes b a))
  | _ -> N

let bytes_text bytes = String.concat " " (List.map (Printf.sprintf "%02x") bytes)

let header (m : Manifest.t) =
  render (O [ ("format", S "i8080-events/1"); ("manifest", S m.id); ("profile", S m.profile) ])

let to_json = function
  | End { tstates; first_t1 } ->
      Some
        (render
           (O
              [
                ("ev", S "end");
                ("tstates", I tstates);
                ("first_t1", match first_t1 with Some n -> I n | None -> N);
              ]))
  | Bus (Bus.Start _) -> None
  | Bus (Bus.Reset { epoch; at }) ->
      Some (render (O [ ("ev", S "reset"); ("epoch", I epoch); ("at", I at) ]))
  | Bus (Bus.Done c) -> Some (render (O (("ev", S "transfer") :: cycle_fields c)))
  | Bus (Bus.Abandoned { cycle; why }) ->
      Some (render (O ((("ev", S "abandoned") :: cycle_fields cycle) @ [ ("why", S why) ])))
  | Bus (Bus.Note { at; msg }) | Instr (Instr.Note { at; msg }) ->
      Some (render (O [ ("ev", S "note"); ("at", I at); ("msg", S msg) ]))
  | Instr (Instr.Halt_evidence { first; samples }) ->
      Some
        (render
           (O
              [
                ("ev", S "halt_evidence");
                ("first", I first);
                ( "samples",
                  A
                    (List.map
                       (fun (n, a, pc) -> O [ ("n", I n); ("a", hex 4 a); ("pc", hex 4 pc) ])
                       samples) );
              ]))
  | Instr (Instr.Instruction i) ->
      Some
        (render
           (O
              [
                ("ev", S "instr");
                ("idx", I i.idx);
                ("epoch", I i.epoch);
                ("pc", hex 4 i.pc);
                ("bytes", S (bytes_text i.bytes));
                ("text", match i.text with Some t -> S t | None -> N);
                ("first", I i.first);
                ("last", I i.last);
                ("tstates", I i.tstates);
                ("transfers", A (List.map (fun (c : Bus.cycle) -> I c.seq) i.accesses));
                ("before", regs_json i.before);
                ("after", regs_json i.after);
                ("changes", changes_json i);
                ("complete", B i.complete);
                ("why", if i.complete then N else S i.why);
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
