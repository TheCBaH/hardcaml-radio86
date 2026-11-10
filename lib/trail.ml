module IM = Map.Make (Int)

type update =
  | Reg of { name : string; before : int option; after : int option }
  | Mem of { addr : int; before : int option; after : int }
  | In of { port : int; value : int }
  | Out of { port : int; value : int }

type step = { instr : Instr.t; updates : update list }

let seen image (c : Bus.cycle) =
  match (c.kind, c.addr, c.data) with
  | (Bus.Fetch | Bus.Mem_read | Bus.Stack_read), Some a, Some d -> IM.add a d image
  | _ -> image

let of_events events =
  let _, rev =
    List.fold_left
      (fun (image, acc) ev ->
        match Events.is_instruction ev with
        | None -> (image, acc)
        | Some (i : Instr.t) ->
            let regs =
              match (i.before, i.after) with
              | Some b, Some a ->
                  List.map
                    (fun (name, before, after) -> Reg { name; before; after })
                    (Regs.changes b a)
              | _ -> []
            in
            let image, rev_io =
              List.fold_left
                (fun (image, upd) (c : Bus.cycle) ->
                  match (c.kind, c.addr, c.data) with
                  | (Bus.Mem_write | Bus.Stack_write), Some addr, Some after ->
                      ( IM.add addr after image,
                        Mem { addr; before = IM.find_opt addr image; after } :: upd )
                  | Bus.Io_read, Some p, Some value ->
                      (image, In { port = p land 0xff; value } :: upd)
                  | Bus.Io_write, Some p, Some value ->
                      (image, Out { port = p land 0xff; value } :: upd)
                  | _ -> (seen image c, upd))
                (image, []) i.accesses
            in
            (image, { instr = i; updates = regs @ List.rev rev_io } :: acc))
      (IM.empty, []) events
  in
  List.rev rev

let hex w = function Some v -> Printf.sprintf "%0*x" w v | None -> "?"
let width_of = function "sp" | "pc" -> 4 | _ -> 2

let render = function
  | Reg { name; before; after } ->
      let w = width_of name in
      Printf.sprintf "%s:%s->%s" name (hex w before) (hex w after)
  | Mem { addr; before; after } -> Printf.sprintf "mem[%04x]:%s->%02x" addr (hex 2 before) after
  | In { port; value } -> Printf.sprintf "in[%02x]=%02x" port value
  | Out { port; value } -> Printf.sprintf "out[%02x]=%02x" port value

let line { instr = i; updates } =
  let state =
    match (i.before, i.after) with
    | Some _, Some _ -> String.concat " " (List.map render updates)
    | _ -> "regs=unobserved " ^ String.concat " " (List.map render updates)
  in
  Printf.sprintf "%s %-16s %s" (hex 4 i.pc) (Option.value i.text ~default:"?") (String.trim state)
  |> String.trim

let lines = List.map line

type final = { regs : Regs.t option; memory : (int * int) list; instructions : int }

let final steps =
  let regs =
    List.fold_left
      (fun acc s -> match s.instr.Instr.after with Some r -> Some r | None -> acc)
      None steps
  in
  let image =
    List.fold_left
      (fun image s ->
        let i = s.instr in
        let image = List.fold_left (fun image c -> seen image c) image i.accesses in
        List.fold_left
          (fun image -> function Mem { addr; after; _ } -> IM.add addr after image | _ -> image)
          image s.updates)
      IM.empty steps
  in
  { regs; memory = IM.bindings image; instructions = List.length steps }

let summary { regs; memory; instructions } =
  let regs_line =
    match regs with
    | None -> "regs unobserved"
    | Some r ->
        "regs "
        ^ String.concat " "
            (List.map
               (fun n -> Printf.sprintf "%s=%s" n (hex (width_of n) (Regs.get r n)))
               Regs.names)
  in
  let runs =
    List.fold_left
      (fun acc (a, d) ->
        match acc with
        | (start, bytes) :: rest when start + List.length bytes = a ->
            (start, bytes @ [ d ]) :: rest
        | _ -> (a, [ d ]) :: acc)
      [] memory
    |> List.rev
  in
  [ Printf.sprintf "%d instructions" instructions; regs_line ]
  @ List.map
      (fun (a, bytes) ->
        Printf.sprintf "mem %04x: %s" a (String.concat " " (List.map (Printf.sprintf "%02x") bytes)))
      runs
