open Ut

let max = 1 lsl 16 - 1

let cap n = n land max

type value =
  | Num of int
  | Wire of string

type expr =
  | Value of value
  | Not of value
  | And of value * value
  | Or of value * value
  | Lshift of value * value
  | Rshift of value * value

type graph = (string * expr) list

let parse str =
  let pv n =
    try Num (int_of_string n)
    with _ -> Wire n
  in
  match String.split_on_char ' ' str with
  | [ n; "->"; x ] -> x, Value (pv n)
  | [ "NOT"; n; "->"; x ] -> x, Not (pv n)
  | [ a; op; b; "->"; x ] -> begin
      x,
      match op with
      | "AND" -> And (pv a, pv b)
      | "OR" -> Or (pv a, pv b)
      | "LSHIFT" -> Lshift (pv a, pv b)
      | "RSHIFT" -> Rshift (pv a, pv b)
      | _ -> assert false
    end
  | _ -> assert false

let simul gates wire =
  let tbl = Hashtbl.create 100 in
  let rec gate x =
    if Hashtbl.mem tbl x then
      Hashtbl.find tbl x
    else
      let v =
        match List.assoc x gates with
        | Value v -> value v
        | Not v -> lnot (value v)
        | And (a, b) -> value a land value b
        | Or (a, b) -> value a lor value b
        | Lshift (a, b) -> value a lsl value b
        | Rshift (a, b) -> value a lsr value b
      in
      let v = cap v in
      Hashtbl.add tbl x v;
      v

  and value = function
    | Num n -> n
    | Wire x -> gate x
  in
  gate wire

let () =
  let data = IO.read_lines () |> List.map parse in
  begin
    (* PART 1 *)
    let a = simul data "a" in
    Printf.printf "%d\n" a;

    (* PART 2 *)
    let data = ("b", Value (Num a)) :: data in
    simul data "a" |> Printf.printf "%d\n";
  end
