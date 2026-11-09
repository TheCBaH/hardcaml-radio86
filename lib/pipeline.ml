type t = { bus : Bus.t; instr : Instr.state; count : int; first_t1 : int option }

let init rule = { bus = Bus.init; instr = Instr.init rule; count = 0; first_t1 = None }

let step t (f : Frame.t) =
  let bus, bouts = Bus.step t.bus f in
  let instr, iouts = Instr.step t.instr f bouts in
  let first_t1 =
    match t.first_t1 with
    | Some _ as x -> x
    | None -> List.find_map (function Bus.Start { at; _ } -> Some at | _ -> None) bouts
  in
  ( { bus; instr; count = t.count + 1; first_t1 },
    List.map (fun o -> Events.Bus o) bouts @ List.map (fun o -> Events.Instr o) iouts )

let finish t =
  let bouts = Bus.finish t.bus in
  List.map (fun o -> Events.Bus o) bouts
  @ List.map (fun o -> Events.Instr o) (Instr.finish t.instr bouts)
  @ [ Events.End { tstates = t.count; first_t1 = t.first_t1 } ]

let run rule frames =
  let t, rev =
    Seq.fold_left
      (fun (t, acc) f ->
        let t, evs = step t f in
        (t, List.rev_append evs acc))
      (init rule, [])
      frames
  in
  List.rev_append rev (finish t)
