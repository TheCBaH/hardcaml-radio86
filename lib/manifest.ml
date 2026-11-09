type rule = After_rising | After_falling | Before_rising
type clock = { path : string list; rule : rule; enable : string list option }
type kind = Pin | Register
type signal = { name : string; path : string list; width : int; kind : kind }
type snapshot = Fetch_t1 | Fetch_accepted
type trace_ref = { file : string; format : string; sha256 : string; bytes : int }

type t = {
  id : string;
  profile : string;
  description : string;
  clock : clock;
  signals : signal list;
  snapshot : snapshot;
  trace : trace_ref option;
}

let format = "i8080-observation-manifest/1"
let profile = "i8080a/1"

let pin_names =
  [
    ("a", 16);
    ("do", 8);
    ("di", 8);
    ("sync", 1);
    ("dbin", 1);
    ("wr_n", 1);
    ("wait", 1);
    ("hlda", 1);
    ("inte", 1);
    ("ready", 1);
    ("hold", 1);
    ("int", 1);
    ("reset", 1);
  ]

let register_names =
  [
    ("acc", 8, true);
    ("bc", 16, true);
    ("de", 16, true);
    ("hl", 16, true);
    ("sp", 16, true);
    ("pc", 16, true);
    ("wz", 16, false);
    ("z", 1, true);
    ("s", 1, true);
    ("p", 1, true);
    ("c", 1, true);
    ("ac", 1, true);
    ("xchg", 1, false);
  ]

let find t name = List.find_opt (fun s -> s.name = name) t.signals
let has_registers t = List.exists (fun s -> s.kind = Register) t.signals

let rule_name = function
  | After_rising -> "after_rising"
  | After_falling -> "after_falling"
  | Before_rising -> "before_rising"

let validate t =
  let errs = ref [] in
  let err fmt = Printf.ksprintf (fun s -> errs := s :: !errs) fmt in
  if t.profile <> profile then err "profile %S is not %S" t.profile profile;
  let seen = Hashtbl.create 32 in
  List.iter
    (fun s ->
      if Hashtbl.mem seen s.name then err "signal %s is declared twice" s.name;
      Hashtbl.replace seen s.name ();
      if s.path = [] then err "signal %s has an empty path" s.name;
      let table =
        match s.kind with
        | Pin -> List.map (fun (n, w) -> (n, w)) pin_names
        | Register -> List.map (fun (n, w, _) -> (n, w)) register_names
      in
      match List.assoc_opt s.name table with
      | None ->
          err "%s is not a %s of the profile" s.name (if s.kind = Pin then "pin" else "register")
      | Some w ->
          if w <> s.width then err "signal %s: width %d, the profile has %d" s.name s.width w)
    t.signals;
  List.iter
    (fun (n, _) ->
      if not (List.exists (fun s -> s.name = n && s.kind = Pin) t.signals) then
        err "pin %s is not declared" n)
    pin_names;
  if has_registers t then
    List.iter
      (fun (n, _, required) ->
        if required && not (List.exists (fun s -> s.name = n && s.kind = Register) t.signals) then
          err "register %s is not declared (registers are all or none)" n)
      register_names;
  if t.clock.path = [] then err "the clock has no path";
  match List.rev !errs with [] -> Ok () | l -> Error l

(* JSON *)

let obj kind f = Jsont.Object.map ~kind f
let finish m = Jsont.Object.finish m
let path_c = Jsont.(list string)

let rule_c =
  Jsont.enum ~kind:"sample rule"
    [
      ("after_rising", After_rising);
      ("after_falling", After_falling);
      ("before_rising", Before_rising);
    ]

let kind_c = Jsont.enum ~kind:"signal kind" [ ("pin", Pin); ("register", Register) ]

let snapshot_c =
  Jsont.enum ~kind:"snapshot rule" [ ("fetch_t1", Fetch_t1); ("fetch_accepted", Fetch_accepted) ]

let enable_c = obj "enable" Fun.id |> Jsont.Object.mem "path" path_c ~enc:Fun.id |> finish

let clock_c =
  obj "clock" (fun path rule enable -> { path; rule; enable })
  |> Jsont.Object.mem "path" path_c ~enc:(fun (c : clock) -> c.path)
  |> Jsont.Object.mem "rule" rule_c ~enc:(fun (c : clock) -> c.rule)
  |> Jsont.Object.opt_mem "enable" enable_c ~enc:(fun (c : clock) -> c.enable)
  |> finish

let signal_c =
  obj "signal" (fun name path width kind -> { name; path; width; kind })
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun (s : signal) -> s.name)
  |> Jsont.Object.mem "path" path_c ~enc:(fun (s : signal) -> s.path)
  |> Jsont.Object.mem "width" Jsont.int ~enc:(fun (s : signal) -> s.width)
  |> Jsont.Object.mem "kind" kind_c ~enc:(fun (s : signal) -> s.kind)
  |> finish

let trace_c =
  obj "trace" (fun file format sha256 bytes -> { file; format; sha256; bytes })
  |> Jsont.Object.mem "file" Jsont.string ~enc:(fun t -> t.file)
  |> Jsont.Object.mem "format" Jsont.string ~enc:(fun t -> t.format)
  |> Jsont.Object.mem "sha256" Jsont.string ~enc:(fun t -> t.sha256)
  |> Jsont.Object.mem "bytes" Jsont.int ~enc:(fun t -> t.bytes)
  |> finish

let format_c = Jsont.enum ~kind:"format" [ (format, ()) ]

let jsont =
  obj "observation manifest" (fun () id profile description clock snapshot signals trace ->
      { id; profile; description; clock; snapshot; signals; trace })
  |> Jsont.Object.mem "format" format_c ~enc:(fun _ -> ())
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun t -> t.id)
  |> Jsont.Object.mem "profile" Jsont.string ~enc:(fun t -> t.profile)
  |> Jsont.Object.mem "description" Jsont.string ~dec_absent:"" ~enc:(fun t -> t.description)
  |> Jsont.Object.mem "clock" clock_c ~enc:(fun t -> t.clock)
  |> Jsont.Object.mem "snapshot" snapshot_c ~enc:(fun t -> t.snapshot)
  |> Jsont.Object.mem "signals" Jsont.(list signal_c) ~enc:(fun t -> t.signals)
  |> Jsont.Object.opt_mem "trace" trace_c ~enc:(fun t -> t.trace)
  |> finish

let of_string ?file s = Jsont_bytesrw.decode_string ?file jsont s

let load file =
  match In_channel.with_open_bin file In_channel.input_all with
  | s -> of_string ~file s
  | exception Sys_error e -> Error e

let to_string t =
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent jsont t with
  | Ok s -> s ^ "\n"
  | Error e -> failwith e
