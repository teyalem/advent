type mstate = {
  mutable pc: int;
  mutable acc: int;
  visited: bool array;
} (* machine state *)

type inst = string * int (* instruction *)

(* 
 * ABSTRACTION FUNCTIONS
 *)

let new_state inst_len = {
  pc = 0;
  acc = 0;
  visited = Array.init inst_len (fun _ -> false);
}

let inst op n = op, n

(* string to instruction *)
let parse_inst str =
  Scanf.sscanf str "%s %d" (fun a b -> inst a b)

(*
 * INSTRUCTIONS
 *)

let inst_acc m n = begin
  m.acc <- m.acc + n;
  m.pc <- m.pc + 1
end

let inst_jmp m n =
  m.pc <- m.pc + n

let inst_nop m = m.pc <- m.pc + 1

exception Not_a_instruction

(* do a instruction in insts *)
let step_machine (m: mstate) (insts: inst array) : unit = begin
  m.visited.(m.pc) <- true; (* mark visited address *)
  match insts.(m.pc) with
  | "acc", n -> inst_acc m n
  | "jmp", n -> inst_jmp m n
  | "nop", _ -> inst_nop m
  | _ -> raise Not_a_instruction
end

(* run machine with program insts.
 * None means there's infinite loop.
 * Some n means the program exits successfully and n is the value of acc.
 *)
let rec run_machine (m: mstate) (insts: inst array) : int option =
  if m.pc >= (Array.length insts) then Some m.acc (* program exited successfully *)
  else if m.visited.(m.pc) then None (* infinite loop detected *)
  else begin (* running state *)
    step_machine m insts;
    run_machine m insts
  end

(* try fix the program by changing a operation at address i.
 * if the operation at address i is "acc", returns None (because acc doesn't make problem).
 * otherwise, returns (probably) fixed instruction array.
 *)
let fix_opt (i: int) (insts: inst array) : inst array option =
  match insts.(i) with
  | "acc", _ -> None
  | op, n ->
    let new_insts = Array.copy insts in
    new_insts.(i) <- (
      (if op = "jmp"
       then "nop"
       else "jmp"),
      n);
    Some new_insts

let main path =
  let insts = Util.read_lines (open_in path)
              |> List.map parse_inst
              |> Array.of_list
  in
  let len = Array.length insts in
  for i = 0 to len-1 do
    fix_opt i insts
    |> fun o -> Option.bind o (run_machine (new_state len))
    |> Option.iter print_int
  done

let _ = Arg.parse [] main ""
