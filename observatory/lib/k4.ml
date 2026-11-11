let hex_cell w s =
  if String.length s = 0 then Error "empty cell"
  else if s.[0] = '~' || s.[0] = 'z' then Ok None
  else
    match int_of_string_opt ("0x" ^ s) with
    | Some v when String.length s = w -> Ok (Some v)
    | _ -> Error (Printf.sprintf "bad cell %S" s)

let bit_cell s =
  match s with
  | "0" -> Ok (Some false)
  | "1" -> Ok (Some true)
  | "~" -> Ok None
  | _ -> Error (Printf.sprintf "bad bit %S" s)

let ( let* ) = Result.bind

let parse text =
  let rows = ref [] and err = ref None in
  List.iter
    (fun raw ->
      if !err = None then begin
        let line =
          match String.index_opt raw '#' with Some i -> String.sub raw 0 i | None -> raw
        in
        let toks = List.filter (( <> ) "") (String.split_on_char ' ' (String.trim line)) in
        if toks <> [] then
          match toks with
          | [ n; a; dout; din; sync; dbin; wr_n; wait; hlda; inte; ready; hold; int_; reset ] -> (
              let r =
                let* n = Option.to_result ~none:"bad n" (int_of_string_opt n) in
                let* a = hex_cell 4 a in
                let* dout = hex_cell 2 dout in
                let* din = hex_cell 2 din in
                let* sync = bit_cell sync in
                let* dbin = bit_cell dbin in
                let* wr_n = bit_cell wr_n in
                let* wait = bit_cell wait in
                let* hlda = bit_cell hlda in
                let* inte = bit_cell inte in
                let* ready = bit_cell ready in
                let* hold = bit_cell hold in
                let* int_ = bit_cell int_ in
                let* reset = bit_cell reset in
                Ok
                  {
                    Frame.n;
                    time = Int64.of_int n;
                    a;
                    dout;
                    din;
                    sync;
                    dbin;
                    wr_n;
                    wait;
                    hlda;
                    inte;
                    ready;
                    hold;
                    int_;
                    reset;
                    regs = None;
                  }
              in
              match r with
              | Ok f -> rows := f :: !rows
              | Error e -> err := Some (Printf.sprintf "%s in %S" e raw))
          | _ -> err := Some (Printf.sprintf "expected 14 fields in %S" raw)
      end)
    (String.split_on_char '\n' text);
  match !err with Some e -> Error e | None -> Ok (List.rev !rows)

type diff = string

let compare_transfers ~expected ~actual ~offset =
  let diffs = ref [] in
  let add fmt = Printf.ksprintf (fun s -> diffs := s :: !diffs) fmt in
  if List.length expected <> List.length actual then
    add "%d transfers expected, %d observed" (List.length expected) (List.length actual);
  List.iteri
    (fun i (e : Bus.cycle) ->
      match List.nth_opt actual i with
      | None -> ()
      | Some (a : Bus.cycle) ->
          let a_first = a.first - offset and a_last = a.last - offset in
          if e.kind <> a.kind then
            add "transfer %d: kind %s, observed %s" i (Bus.kind_name e.kind) (Bus.kind_name a.kind);
          if e.first <> a_first || e.last <> a_last then
            add "transfer %d: T%d-%d, observed T%d-%d" i e.first e.last a_first a_last;
          if e.waits <> a.waits then add "transfer %d: %d waits, observed %d" i e.waits a.waits;
          let cmp name x y =
            match (x, y) with
            | Some x, Some y when x <> y -> add "transfer %d: %s %x, observed %x" i name x y
            | _ -> ()
          in
          (* the address of a halt acknowledge is not compared: vm80a shows the address of HLT, the
             hand trace and the Hardcaml core the one after it (decision N7 is open) *)
          if e.kind <> Bus.Halt_ack then cmp "address" e.addr a.addr;
          cmp "data" e.data a.data)
    expected;
  List.rev !diffs

let first_t1 evs =
  List.find_map (function Events.Bus (Bus.Start { at; _ }) -> Some at | _ -> None) evs

(* A hand-written or recorded pin trace against the samples of a run, row by row. A cell the expected
   row masks ([~], or [zz]) is not compared; a cell it gives must be given, and equal, in the sample. *)
let compare_pins ~(expected : Frame.t list) ~(actual : Frame.t list) =
  let diffs = ref [] in
  let add fmt =
    Printf.ksprintf (fun s -> if List.length !diffs < 20 then diffs := s :: !diffs) fmt
  in
  if List.length expected <> List.length actual then
    add "%d rows expected, %d samples" (List.length expected) (List.length actual);
  let rec go e a =
    match (e, a) with
    | (e : Frame.t) :: er, (a : Frame.t) :: ar ->
        let cmp name x y =
          match (x, y) with
          | Some x, Some y when x <> y -> add "n=%d %s: expected %x, got %x" e.n name x y
          | Some x, None -> add "n=%d %s: expected %x, got an unknown value" e.n name x
          | _ -> ()
        in
        let cmpb name x y = cmp name (Option.map Bool.to_int x) (Option.map Bool.to_int y) in
        cmp "a" e.a a.a;
        cmp "do" e.dout a.dout;
        cmp "di" e.din a.din;
        cmpb "sync" e.sync a.sync;
        cmpb "dbin" e.dbin a.dbin;
        cmpb "wr_n" e.wr_n a.wr_n;
        cmpb "wait" e.wait a.wait;
        cmpb "hlda" e.hlda a.hlda;
        cmpb "inte" e.inte a.inte;
        cmpb "ready" e.ready a.ready;
        cmpb "hold" e.hold a.hold;
        cmpb "int" e.int_ a.int_;
        cmpb "reset" e.reset a.reset;
        go er ar
    | _ -> ()
  in
  go expected actual;
  List.rev !diffs
