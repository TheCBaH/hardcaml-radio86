type probe = { name : string; path : string list; width : int }

let signals tr = List.of_seq (Vcd_signals.of_header (Hwfrp.Trace.header tr))

let probes (m : Manifest.t) =
  let p name path width = { name; path; width } in
  (p "clock" m.clock.path 1 :: Option.to_list (Option.map (fun e -> p "enable" e 1) m.clock.enable))
  @ List.concat_map
      (fun (s : Manifest.signal) ->
        p s.name s.path s.width
        :: Option.to_list (Option.map (fun v -> p (s.name ^ ".valid") v 1) s.valid))
      m.signals

type found = Present | Moved of Vcd_signals.t list | Missing

let leaf = function [] -> "" | l -> List.nth l (List.length l - 1)

let trim_underscores s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = '_' do
    decr n
  done;
  String.sub s 0 !n

let normal s =
  let s = String.lowercase_ascii s in
  trim_underscores
    (if String.length s > 4 && String.sub s 0 4 = "pin_" then String.sub s 4 (String.length s - 4)
     else s)

let find sigs p =
  if List.exists (fun (s : Vcd_signals.t) -> s.parts = p.path && s.width = p.width) sigs then
    Present
  else
    let want = normal (leaf p.path) in
    match
      List.filter
        (fun (s : Vcd_signals.t) -> s.width = p.width && normal (leaf s.parts) = want)
        sigs
    with
    | [] -> Missing
    | l -> Moved l

let check m sigs = List.map (fun p -> (p, find sigs p)) (probes m)
let dotted = String.concat "."

let hint sigs p =
  match find sigs p with
  | Present -> None
  | Moved l ->
      Some
        (Printf.sprintf "%s: declared as %s" p.name
           (String.concat ", " (List.map (fun (s : Vcd_signals.t) -> s.path) l)))
  | Missing ->
      let like =
        List.filter
          (fun (s : Vcd_signals.t) ->
            let l = normal (leaf s.parts) and w = normal (leaf p.path) in
            l <> "" && w <> "" && (String.starts_with ~prefix:w l || String.starts_with ~prefix:l w))
          sigs
      in
      if like = [] then None
      else
        Some
          (Printf.sprintf "%s: nearest by name %s" p.name
             (String.concat ", "
                (List.map (fun (s : Vcd_signals.t) -> Printf.sprintf "%s (%d)" s.path s.width) like)))

let repath (m : Manifest.t) sigs =
  let results = check m sigs in
  let errs =
    List.filter_map
      (fun (p, f) ->
        match f with
        | Present | Moved [ _ ] -> None
        | Moved l ->
            Some
              (Printf.sprintf "%s: %d candidates: %s" p.name (List.length l)
                 (String.concat ", " (List.map (fun (s : Vcd_signals.t) -> s.path) l)))
        | Missing -> Some (Printf.sprintf "%s: %s is not declared" p.name (dotted p.path)))
      results
  in
  if errs <> [] then Error errs
  else
    let target = Hashtbl.create 32 in
    List.iter
      (fun (p, f) ->
        match f with
        | Moved [ s ] -> Hashtbl.replace target (p.path, p.width) s.Vcd_signals.parts
        | _ -> ())
      results;
    let get w path = Option.value (Hashtbl.find_opt target (path, w)) ~default:path in
    Ok
      {
        m with
        clock =
          { m.clock with path = get 1 m.clock.path; enable = Option.map (get 1) m.clock.enable };
        signals =
          List.map
            (fun (s : Manifest.signal) ->
              { s with path = get s.width s.path; valid = Option.map (get 1) s.valid })
            m.signals;
      }
