open Ut

type vm = {
  regs: (reg, int) Hashtbl.t;
  mutable ip: int;
  prog: inst array;
}

and reg = string

and inst =
  | Set of reg * value
  | Sub of reg * value
  | Mul of reg * value
  | Jnz of value * value

and value =
  | Reg of reg
  | Int of int

let parse str =
  let is_reg str = 'a' <= str.[0] && str.[0] <= 'z' in
  let pv str = if is_reg str then Reg str else Int (int_of_string str) in
  Scanf.sscanf str "%s %s %s" (fun op x y ->
      match op with
      | "set" -> Set (x, pv y)
      | "sub" -> Sub (x, pv y)
      | "mul" -> Mul (x, pv y)
      | "jnz" -> Jnz (pv x, pv y)
      | _ -> assert false)

let make_vm prog =
  { regs = Hashtbl.create 100;
    ip = 0;
    prog; }

let get_value { regs; _ } = function
  | Reg r -> Hashtbl.find_opt regs r |> Option.value ~default: 0
  | Int n -> n

let set vm r v =
  Hashtbl.replace vm.regs r v

let tick vm n =
  vm.ip <- vm.ip + n

let step cnt vm =
  let get = get_value vm
  and reg r = get_value vm (Reg r)
  and set = set vm
  and tick = tick vm in
  function
  | Set (r, v) -> set r @@ get v; tick 1
  | Sub (r, v) -> set r @@ reg r - get v; tick 1
  | Mul (r, v) -> incr cnt; set r @@ reg r * get v; tick 1
  | Jnz (v, offset) -> if get v <> 0 then vm.ip <- vm.ip + get offset else tick 1

let part1 prog =
  let vm = make_vm prog in
  let cnt = ref 0 in
  let f = step cnt vm in
  let rec aux () =
    if vm.ip < Array.length vm.prog then begin
      f vm.prog.(vm.ip);
      aux ()
    end
  in
  aux ();
  !cnt

let is_prime n =
  let rec aux i =
    if i > truncate @@ sqrt @@ float n then true
    else if n mod i = 0 then false
    else aux @@ i + 1
  in
  aux 2

let pull_data prog =
  match prog.(0) with
  | Set ("b", Int n) -> n
  | _ -> assert false

let part2 n =
  let b = n * 100 + 100_000 in
  Seq.init 1001 (fun i -> b + 17 * i)
  |> Seq.filter (Fun.negate is_prime)
  |> Seq.length

let () =
  let data = IO.read_lines () |> List.map parse |> Array.of_list in
  (* PART 1 *) part1 data |> print_int;
  print_newline ();
  (* PART 2 *) pull_data data |> part2 |> print_int;
