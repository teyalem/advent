open Ut

type vm = {
  regs: (reg, int) Hashtbl.t;
  mutable ip: int;
  prog: inst array;
  mutable last_sound: int;
  queue: int Queue.t;
}

and reg = string

and inst =
  | Snd of value
  | Set of reg * value
  | Add of reg * value
  | Mul of reg * value
  | Mod of reg * value
  | Rcv of reg
  | Jgz of value * value

and value =
  | Reg of reg
  | Int of int

let parse str =
  let is_reg str = 'a' <= str.[0] && str.[0] <= 'z' in
  let pv str = if is_reg str then Reg str else Int (int_of_string str) in
  Scanf.sscanf str "%s %s %s" (fun op x y ->
      match op with
      | "snd" -> Snd (pv x)
      | "set" -> Set (x, pv y)
      | "add" -> Add (x, pv y)
      | "mul" -> Mul (x, pv y)
      | "mod" -> Mod (x, pv y)
      | "rcv" -> Rcv x
      | "jgz" -> Jgz (pv x, pv y)
      | _ -> assert false)

let make_vm prog =
  { regs = Hashtbl.create 100;
    ip = 0;
    prog;
    last_sound = 0;
    queue = Queue.create (); }

let get_value { regs; _ } = function
  | Reg r -> Hashtbl.find_opt regs r |> Option.value ~default: 0
  | Int n -> n

let set vm r v =
  Hashtbl.replace vm.regs r v

let tick vm n =
  vm.ip <- vm.ip + n

let step vm =
  let get = get_value vm
  and reg r = get_value vm (Reg r)
  and set = set vm
  and tick = tick vm in
  function
  | Set (r, v) -> set r @@ get v; tick 1
  | Add (r, v) -> set r @@ reg r + get v; tick 1
  | Mul (r, v) -> set r @@ reg r * get v; tick 1
  | Mod (r, v) -> set r @@ reg r mod get v; tick 1
  | Jgz (v, offset) -> if get v > 0 then vm.ip <- vm.ip + get offset else tick 1
  | _ -> failwith "unimplemented"

let part1 prog =
  let vm = make_vm prog in
  let step vm =
    let f = step vm in
    match vm.prog.(vm.ip) with
    | Snd v -> vm.last_sound <- get_value vm v; tick vm 1; true
    | Rcv r ->
      if get_value vm (Reg r) <> 0
      then false
      else begin tick vm 1; true end
    | i -> f i; true
  in
  let rec aux vm =
    if step vm then aux vm else vm.last_sound
  in
  aux vm

let part2 prog =
  let vm0 = make_vm prog
  and vm1 = make_vm prog in
  Hashtbl.replace vm1.regs "p" 1;
  let cnt0 = ref 0 and cnt1 = ref 0 in

  let step cnt vm other =
    let f = step vm in
    let inst = vm.prog.(vm.ip) in
    match inst with
    | Snd v ->
      Queue.push (get_value vm v) other.queue;
      tick vm 1;
      incr cnt;
      true
    | Rcv r ->
      if Queue.is_empty vm.queue then false
      else begin
        set vm r @@ Queue.pop vm.queue;
        tick vm 1;
        true
      end
    | i -> f i; true
  in

  let rec run cnt vm other moved =
    if step cnt vm other
    then run cnt vm other true
    else moved
  in

  let rec aux () =
    if run cnt0 vm0 vm1 false || run cnt1 vm1 vm0 false
    then aux ()
    else () (* deadlock *)
  in
  aux ();
  !cnt1

let () =
  let data = IO.read_lines () |> List.map parse |> Array.of_list in
  (* PART 1 *) part1 data |> print_int;
  print_newline ();
  (* PART 2 *) part2 data |> print_int;
