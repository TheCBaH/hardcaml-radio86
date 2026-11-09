let esc s =
  let b = Buffer.create (String.length s + 2) in
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | c when Char.code c < 0x20 -> Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let hexo w = function None -> "~" | Some v -> Printf.sprintf "%0*x" w v

let render (m : Manifest.t) evs =
  let out = ref [] in
  let add fmt = Printf.ksprintf (fun s -> out := s :: !out) fmt in
  add {|{"ph":"M","pid":1,"name":"process_name","args":{"name":"i8080 %s"}}|} (esc m.id);
  List.iter
    (fun (tid, name) ->
      add {|{"ph":"M","pid":1,"tid":%d,"name":"thread_name","args":{"name":"%s"}}|} tid name)
    [ (1, "instructions"); (2, "bus"); (3, "control") ];
  let args kvs =
    String.concat "," (List.map (fun (k, v) -> Printf.sprintf {|"%s":"%s"|} k (esc v)) kvs)
  in
  List.iter
    (function
      | Events.Bus (Bus.Reset { epoch; at }) ->
          add {|{"ph":"i","s":"t","pid":1,"tid":3,"ts":%d,"name":"reset","args":{%s}}|} at
            (args [ ("epoch", string_of_int epoch) ])
      | Events.Bus (Bus.Done c) ->
          let name =
            match c.kind with
            | Bus.Halt_ack -> Printf.sprintf "halt_ack %s" (hexo 4 c.addr)
            | k -> Printf.sprintf "%s %s=%s" (Bus.kind_name k) (hexo 4 c.addr) (hexo 2 c.data)
          in
          add {|{"ph":"X","pid":1,"tid":2,"ts":%d,"dur":%d,"name":"%s","args":{%s}}|} c.first
            (c.last - c.first + 1)
            (esc name)
            (args [ ("waits", string_of_int c.waits); ("seq", string_of_int c.seq) ])
      | Events.Bus (Bus.Abandoned { cycle = c; why }) ->
          add {|{"ph":"X","pid":1,"tid":2,"ts":%d,"dur":%d,"name":"abandoned %s","args":{%s}}|}
            c.first
            (max 1 (c.last - c.first + 1))
            (Bus.kind_name c.kind)
            (args [ ("why", why) ])
      | Events.Bus (Bus.Note { at; msg }) | Events.Instr (Instr.Note { at; msg }) ->
          add {|{"ph":"i","s":"t","pid":1,"tid":3,"ts":%d,"name":"note","args":{%s}}|} at
            (args [ ("msg", msg) ])
      | Events.Bus (Bus.Start _) -> ()
      | Events.Instr (Instr.Halt_evidence { first; samples }) ->
          add {|{"ph":"i","s":"t","pid":1,"tid":3,"ts":%d,"name":"halt cycle","args":{%s}}|} first
            (args
               (List.map
                  (fun (n, a, pc) ->
                    (Printf.sprintf "n%d" n, Printf.sprintf "a=%s pc=%s" (hexo 4 a) (hexo 4 pc)))
                  samples))
      | Events.Instr (Instr.Instruction i) -> (
          add {|{"ph":"X","pid":1,"tid":1,"ts":%d,"dur":%d,"name":"%s","args":{%s}}|} i.first
            (i.last - i.first + 1)
            (esc (Option.value i.text ~default:"?"))
            (args
               [
                 ("pc", hexo 4 i.pc);
                 ("bytes", String.concat " " (List.map (Printf.sprintf "%02x") i.bytes));
                 ("complete", string_of_bool i.complete);
               ]);
          match i.after with
          | Some r ->
              List.iter
                (fun n ->
                  match Regs.get r n with
                  | Some v ->
                      add {|{"ph":"C","pid":1,"ts":%d,"name":"reg %s","args":{"value":%d}}|} i.last
                        n v
                  | None -> ())
                Regs.names
          | None -> ())
      | Events.End _ -> ())
    evs;
  "{\"traceEvents\":[\n" ^ String.concat ",\n" (List.rev !out) ^ "\n]}\n"
