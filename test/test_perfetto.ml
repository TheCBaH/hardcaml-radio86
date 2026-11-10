open I8080_trace
open Helpers

let%expect_test "the Perfetto lanes of the hand trace are JSON" =
  let m = manifest () in
  let evs = Pipeline.run m.snapshot (List.to_seq (frames_of Traces.nowait)) in
  let text = Perfetto.render m evs in
  (match Jsont_bytesrw.decode_string Jsont.json text with
  | Ok _ -> print_endline "valid JSON"
  | Error e -> print_endline e);
  print_string text;
  [%expect
    {|
    valid JSON
    {"displayTimeUnit":"ns","traceEvents":[
    {"ph":"M","pid":1,"name":"process_name","args":{"name":"i8080 vm80a-fst"}},
    {"ph":"M","pid":1,"tid":1,"name":"thread_name","args":{"name":"instructions"}},
    {"ph":"M","pid":1,"tid":2,"name":"thread_name","args":{"name":"bus"}},
    {"ph":"M","pid":1,"tid":3,"name":"thread_name","args":{"name":"control"}},
    {"ph":"X","pid":1,"tid":2,"name":"fetch 0000=3e","ts":0,"dur":3,"args":{"waits":"0","seq":"0"}},
    {"ph":"X","pid":1,"tid":2,"name":"mem_read 0001=2a","ts":4,"dur":3,"args":{"waits":"0","seq":"1"}},
    {"ph":"X","pid":1,"tid":2,"name":"fetch 0002=32","ts":7,"dur":3,"args":{"waits":"0","seq":"2"}},
    {"ph":"X","pid":1,"tid":1,"name":"mvi a, 0x2a","ts":0,"dur":7,"args":{"pc":"0000","bytes":"3e 2a","complete":"true"}},
    {"ph":"X","pid":1,"tid":2,"name":"mem_read 0003=00","ts":11,"dur":3,"args":{"waits":"0","seq":"3"}},
    {"ph":"X","pid":1,"tid":2,"name":"mem_read 0004=20","ts":14,"dur":3,"args":{"waits":"0","seq":"4"}},
    {"ph":"X","pid":1,"tid":2,"name":"mem_write 2000=2a","ts":17,"dur":3,"args":{"waits":"0","seq":"5"}},
    {"ph":"X","pid":1,"tid":2,"name":"fetch 0005=76","ts":20,"dur":3,"args":{"waits":"0","seq":"6"}},
    {"ph":"X","pid":1,"tid":1,"name":"sta 0x2000","ts":7,"dur":13,"args":{"pc":"0002","bytes":"32 00 20","complete":"true"}},
    {"ph":"X","pid":1,"tid":2,"name":"halt_ack ~","ts":24,"dur":3,"args":{"waits":"0","seq":"7"}},
    {"ph":"X","pid":1,"tid":1,"name":"hlt","ts":20,"dur":7,"args":{"pc":"0005","bytes":"76","complete":"true"}},
    {"ph":"i","s":"t","pid":1,"tid":3,"name":"halt cycle","ts":24,"args":{"n18":"a=2000 pc=~","n19":"a=~ pc=~","n20":"a=0005 pc=~","n21":"a=0005 pc=~","n22":"a=0005 pc=~","n23":"a=~ pc=~","n24":"a=~ pc=~","n25":"a=~ pc=~","n26":"a=~ pc=~","n27":"a=~ pc=~","n28":"a=~ pc=~"}}
    ]}
    |}]

let%expect_test "the JSON lines of the hand trace" =
  let m = manifest () in
  let evs = Pipeline.run m.snapshot (List.to_seq (frames_of Traces.wait2)) in
  print_string (Export.jsonl m evs);
  [%expect
    {|
    {"format":"i8080-events/1","manifest":"vm80a-fst","profile":"i8080a/1"}
    {"ev":"transfer","epoch":0,"seq":0,"kind":"fetch","status":"0xa2","addr":"0x0000","data":"0x3e","first":0,"last":2,"waits":0}
    {"ev":"transfer","epoch":0,"seq":1,"kind":"mem_read","status":"0x82","addr":"0x0001","data":"0x2a","first":4,"last":6,"waits":0}
    {"ev":"transfer","epoch":0,"seq":2,"kind":"fetch","status":"0xa2","addr":"0x0002","data":"0x32","first":7,"last":9,"waits":0}
    {"ev":"instr","idx":0,"epoch":0,"pc":"0x0000","bytes":"3e 2a","text":"mvi a, 0x2a","first":0,"last":6,"tstates":7,"transfers":[0,1],"before":null,"after":null,"changes":null,"complete":true,"why":null}
    {"ev":"transfer","epoch":0,"seq":3,"kind":"mem_read","status":"0x82","addr":"0x0003","data":"0x00","first":11,"last":13,"waits":0}
    {"ev":"transfer","epoch":0,"seq":4,"kind":"mem_read","status":"0x82","addr":"0x0004","data":"0x20","first":14,"last":16,"waits":0}
    {"ev":"transfer","epoch":0,"seq":5,"kind":"mem_write","status":"0x00","addr":"0x2000","data":"0x2a","first":17,"last":21,"waits":2}
    {"ev":"transfer","epoch":0,"seq":6,"kind":"fetch","status":"0xa2","addr":"0x0005","data":"0x76","first":22,"last":24,"waits":0}
    {"ev":"instr","idx":1,"epoch":0,"pc":"0x0002","bytes":"32 00 20","text":"sta 0x2000","first":7,"last":21,"tstates":15,"transfers":[2,3,4,5],"before":null,"after":null,"changes":null,"complete":true,"why":null}
    {"ev":"transfer","epoch":0,"seq":7,"kind":"halt_ack","status":"0x8a","addr":null,"data":null,"first":26,"last":28,"waits":0}
    {"ev":"instr","idx":2,"epoch":0,"pc":"0x0005","bytes":"76","text":"hlt","first":22,"last":28,"tstates":7,"transfers":[6,7],"before":null,"after":null,"changes":null,"complete":true,"why":null}
    {"ev":"halt_evidence","first":26,"samples":[{"n":20,"a":"0x2000","pc":null},{"n":21,"a":null,"pc":null},{"n":22,"a":"0x0005","pc":null},{"n":23,"a":"0x0005","pc":null},{"n":24,"a":"0x0005","pc":null},{"n":25,"a":null,"pc":null},{"n":26,"a":null,"pc":null},{"n":27,"a":null,"pc":null},{"n":28,"a":null,"pc":null},{"n":29,"a":null,"pc":null},{"n":30,"a":null,"pc":null}]}
    {"ev":"end","tstates":31,"first_t1":0} |}]
