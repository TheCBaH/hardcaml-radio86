(* The bus-visible machine cycles of every opcode, as plain data: what [Core] builds its sequencing ROM
   from and what the tests compare with Rivet's timing table.

   Cycles are the ones the pins show (one SYNC each). The 8080's internal cycles (DAD's two, the fifth
   state of PUSH, CALL, RST, Rcc, and of XTHL's last write) are extra T-states of the cycle that
   precedes them, as in vm80a: [len] is the number of T-states of the cycle without waits. *)

type src = Pc | Hl | Bc | De | Sp | Wz

let src_code = function Pc -> 0 | Hl -> 1 | Bc -> 2 | De -> 3 | Sp -> 4 | Wz -> 5

type cycle = { status : int; len : int; src : src }

type t = {
  cycles : cycle list;  (** The longest path: a taken Rcc or Ccc. *)
  untaken_end : int option;
      (** Index of the last cycle when the condition of Rcc or Ccc is false. *)
}

let fetch len = { status = 0xa2; len; src = Pc }
let mem_read src = { status = 0x82; len = 3; src }
let mem_write src = { status = 0x00; len = 3; src }
let stack_read = { status = 0x86; len = 3; src = Sp }
let stack_write ?(len = 3) () = { status = 0x04; len; src = Sp }
let halt = { status = 0x8a; len = 3; src = Pc }
let all c = { cycles = c; untaken_end = None }

let plan op =
  let hi = op lsr 6 and r = op land 7 and d = (op lsr 3) land 7 in
  if op = 0x76 then all [ fetch 4; halt ]
  else
    match hi with
    | 1 ->
        all
          (if d = 6 then [ fetch 4; mem_write Hl ]
           else if r = 6 then [ fetch 4; mem_read Hl ]
           else [ fetch 5 ])
    | 2 -> all (if r = 6 then [ fetch 4; mem_read Hl ] else [ fetch 4 ])
    | 0 -> (
        match r with
        | 0 | 7 -> all [ fetch 4 ]
        | 1 -> all (if d land 1 = 0 then [ fetch 4; mem_read Pc; mem_read Pc ] else [ fetch 10 ])
        | 2 -> (
            match d with
            | 0 -> all [ fetch 4; mem_write Bc ]
            | 1 -> all [ fetch 4; mem_read Bc ]
            | 2 -> all [ fetch 4; mem_write De ]
            | 3 -> all [ fetch 4; mem_read De ]
            | 4 -> all [ fetch 4; mem_read Pc; mem_read Pc; mem_write Wz; mem_write Wz ]
            | 5 -> all [ fetch 4; mem_read Pc; mem_read Pc; mem_read Wz; mem_read Wz ]
            | 6 -> all [ fetch 4; mem_read Pc; mem_read Pc; mem_write Wz ]
            | _ -> all [ fetch 4; mem_read Pc; mem_read Pc; mem_read Wz ])
        | 3 -> all [ fetch 5 ]
        | 4 | 5 -> all (if d = 6 then [ fetch 4; mem_read Hl; mem_write Hl ] else [ fetch 5 ])
        | _ ->
            all (if d = 6 then [ fetch 4; mem_read Pc; mem_write Hl ] else [ fetch 4; mem_read Pc ])
        )
    | _ -> (
        match r with
        | 0 -> { cycles = [ fetch 5; stack_read; stack_read ]; untaken_end = Some 0 }
        | 1 -> (
            match d with 5 | 7 -> all [ fetch 5 ] | _ -> all [ fetch 4; stack_read; stack_read ])
        | 2 -> all [ fetch 4; mem_read Pc; mem_read Pc ]
        | 3 -> (
            let io status = { status; len = 3; src = Wz } in
            match d with
            | 0 | 1 -> all [ fetch 4; mem_read Pc; mem_read Pc ]
            | 2 -> all [ fetch 4; mem_read Pc; io 0x10 ]
            | 3 -> all [ fetch 4; mem_read Pc; io 0x42 ]
            | 4 -> all [ fetch 4; stack_read; stack_read; stack_write (); stack_write ~len:5 () ]
            | _ -> all [ fetch 4 ])
        | 4 ->
            {
              cycles = [ fetch 5; mem_read Pc; mem_read Pc; stack_write (); stack_write () ];
              untaken_end = Some 2;
            }
        | 5 ->
            if d land 1 = 0 then all [ fetch 5; stack_write (); stack_write () ]
            else all [ fetch 5; mem_read Pc; mem_read Pc; stack_write (); stack_write () ]
        | 6 -> all [ fetch 4; mem_read Pc ]
        | _ -> all [ fetch 5; stack_write (); stack_write () ])

let table = Array.init 256 plan
let max_cycles = 5
let cycles op = table.(op land 0xff).cycles

(* Cycle [k] of [op]; past the end, a placeholder that the sequencer never reaches. *)
let cycle op k =
  match List.nth_opt (cycles op) k with Some c -> c | None -> { status = 0; len = 3; src = Pc }

let is_last op k = k = List.length (cycles op) - 1
let total l = List.fold_left (fun n c -> n + c.len) 0 l

(* T-states with the condition false and true (equal for unconditional opcodes). *)
let tstates op =
  let p = table.(op land 0xff) in
  let untaken =
    match p.untaken_end with
    | None -> total p.cycles
    | Some k -> total (List.filteri (fun i _ -> i <= k) p.cycles)
  in
  (untaken, total p.cycles)

(* Bytes an opcode takes from the instruction stream: its PC-addressed cycles. *)
let length op = List.length (List.filter (fun c -> c.src = Pc && c.status <> 0x8a) (cycles op))
