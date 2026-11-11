(* K4 stimulus schedule (i8080-stimulus v1): ready, hold, int and reset from a sample on, either an
   absolute sample [n] or anchored on a machine cycle [m<c>+<k>], the bytes successive INs read from a
   port ([io]) and the bytes interrupt-acknowledge cycles supply ([inta]). *)

type signal = Ready | Hold | Int | Reset
type at = Sample of int | Cycle of { cycle : int; offset : int }
type statement = { signal : signal; value : bool; at : at }

type t = {
  events : statement list;
  io : (int * int list) list;  (** port, bytes in the order they are read *)
  inta : int list;
}

let empty = { events = []; io = []; inta = [] }
let signal_name = function Ready -> "ready" | Hold -> "hold" | Int -> "int" | Reset -> "reset"
let default = function Ready -> true | Hold -> false | Int -> false | Reset -> true

let parse_at s =
  let n = String.length s in
  if n < 2 || s.[0] <> '@' then Error (Printf.sprintf "expected @<when>, got %S" s)
  else
    let w = String.sub s 1 (n - 1) in
    match int_of_string_opt w with
    | Some k when k >= 0 -> Ok (Sample k)
    | Some _ -> Error (Printf.sprintf "negative sample in %S" s)
    | None -> (
        match try Some (Scanf.sscanf w "m%d+%d%!" (fun c k -> (c, k))) with _ -> None with
        | Some (cycle, offset) when cycle >= 0 && offset >= 0 -> Ok (Cycle { cycle; offset })
        | _ -> Error (Printf.sprintf "bad <when> %S" s))

let parse_hex tok =
  match int_of_string_opt ("0x" ^ tok) with Some b when b >= 0 && b < 256 -> Some b | _ -> None

let parse text =
  let rec go (acc : t) = function
    | [] -> Ok { acc with events = List.rev acc.events; io = List.rev acc.io }
    | (lineno, raw) :: rest -> (
        let line =
          match String.index_opt raw '#' with Some i -> String.sub raw 0 i | None -> raw
        in
        let toks = List.filter (( <> ) "") (String.split_on_char ' ' (String.trim line)) in
        let fail fmt =
          Printf.ksprintf (fun m -> Error (Printf.sprintf "line %d: %s" lineno m)) fmt
        in
        let bytes l = List.map parse_hex l in
        match toks with
        | [] -> go acc rest
        | "io" :: port :: (_ :: _ as l) -> (
            match (parse_hex port, bytes l) with
            | Some p, bs when List.for_all Option.is_some bs ->
                go { acc with io = (p, List.filter_map Fun.id bs) :: acc.io } rest
            | _ -> fail "bad io statement")
        | "inta" :: (_ :: _ as l) ->
            let bs = bytes l in
            if List.for_all Option.is_some bs then
              go { acc with inta = acc.inta @ List.filter_map Fun.id bs } rest
            else fail "bad inta byte"
        | [ name; value; at ] -> (
            let signal =
              match name with
              | "ready" -> Some Ready
              | "hold" -> Some Hold
              | "int" -> Some Int
              | "reset" -> Some Reset
              | _ -> None
            in
            match (signal, value, parse_at at) with
            | None, _, _ -> fail "unsupported statement %S" name
            | _, _, Error e -> fail "%s" e
            | Some signal, ("0" | "1"), Ok at ->
                go { acc with events = { signal; value = value = "1"; at } :: acc.events } rest
            | _ -> fail "value %S is not 0 or 1" value)
        | name :: _ -> fail "unsupported statement %S" name)
  in
  go empty (List.mapi (fun i l -> (i + 1, l)) (String.split_on_char '\n' text))

(* [t1 c] is the sample of the SYNC that opens machine cycle [c], once seen. The value of a signal in
   sample [n] is the latest statement that starts at or before [n]; a statement anchored on a cycle
   whose T1 is not yet known has not started. *)
let value (t : t) ~(t1 : int -> int option) ~n signal =
  let start s =
    match s.at with
    | Sample k -> Some k
    | Cycle { cycle; offset } -> Option.map (fun s -> s + offset) (t1 cycle)
  in
  List.fold_left
    (fun (best, v) s ->
      if s.signal <> signal then (best, v)
      else match start s with Some k when k <= n && k >= best -> (k, s.value) | _ -> (best, v))
    (-1, default signal)
    t.events
  |> snd
