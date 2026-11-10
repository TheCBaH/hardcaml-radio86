module C = Hwfrp_sinks.Chrome_trace
module J = Hwfrp_sinks.Json

let hexo w = function None -> "~" | Some v -> Printf.sprintf "%0*x" w v
let str kvs = List.map (fun (k, v) -> (k, J.Str v)) kvs

let events evs =
  List.concat_map
    (function
      | Events.Bus (Bus.Reset { epoch; at }) ->
          [
            C.Instant
              {
                lane = "control";
                name = "reset";
                at;
                args = str [ ("epoch", string_of_int epoch) ];
              };
          ]
      | Events.Bus (Bus.Done c) ->
          let name =
            match c.kind with
            | Bus.Halt_ack -> Printf.sprintf "halt_ack %s" (hexo 4 c.addr)
            | k -> Printf.sprintf "%s %s=%s" (Bus.kind_name k) (hexo 4 c.addr) (hexo 2 c.data)
          in
          [
            C.Span
              {
                lane = "bus";
                name;
                start = c.first;
                stop = c.last;
                args = str [ ("waits", string_of_int c.waits); ("seq", string_of_int c.seq) ];
              };
          ]
      | Events.Bus (Bus.Abandoned { cycle = c; why }) ->
          [
            C.Span
              {
                lane = "bus";
                name = Printf.sprintf "abandoned %s" (Bus.kind_name c.kind);
                start = c.first;
                stop = c.last;
                args = str [ ("why", why) ];
              };
          ]
      | Events.Bus (Bus.Note { at; msg }) | Events.Instr (Instr.Note { at; msg }) ->
          [ C.Instant { lane = "control"; name = "note"; at; args = str [ ("msg", msg) ] } ]
      | Events.Bus (Bus.Start _) -> []
      | Events.Instr (Instr.Halt_evidence { first; samples }) ->
          [
            C.Instant
              {
                lane = "control";
                name = "halt cycle";
                at = first;
                args =
                  str
                    (List.map
                       (fun (n, a, pc) ->
                         (Printf.sprintf "n%d" n, Printf.sprintf "a=%s pc=%s" (hexo 4 a) (hexo 4 pc)))
                       samples);
              };
          ]
      | Events.Instr (Instr.Instruction i) ->
          C.Span
            {
              lane = "instructions";
              name = Option.value i.text ~default:"?";
              start = i.first;
              stop = i.last;
              args =
                str
                  [
                    ("pc", hexo 4 i.pc);
                    ("bytes", String.concat " " (List.map (Printf.sprintf "%02x") i.bytes));
                    ("complete", string_of_bool i.complete);
                  ];
            }
          ::
          (match i.after with
          | Some r ->
              List.filter_map
                (fun n ->
                  Option.map
                    (fun v -> C.Counter { name = "reg " ^ n; at = i.last; value = v })
                    (Regs.get r n))
                Regs.names
          | None -> [])
      | Events.End _ -> [])
    evs

let render (m : Manifest.t) evs =
  C.to_string
    [
      {
        C.name = "i8080 " ^ m.id;
        lanes = [ "instructions"; "bus"; "control" ];
        events = events evs;
      };
    ]
