open Ut

type reg = string

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

let pull_data = function
  | Set ("b", Int n) :: _ -> n
  | _ -> assert false

let part1 n =
  (n - 2) * (n - 2)

let is_prime n =
  let rec aux i =
    if i > truncate @@ sqrt @@ float n then true
    else if n mod i = 0 then false
    else aux @@ i + 1
  in
  aux 2

let part2 n =
  let b = n * 100 + 100_000 in
  Seq.init 1001 (fun i -> b + 17 * i)
  |> Seq.filter (Fun.negate is_prime)
  |> Seq.length

let () =
  let data = IO.read_lines () |> List.map parse |> pull_data in
  (* PART 1 *) part1 data |> print_int;
  print_newline ();
  (* PART 2 *) part2 data |> print_int;
