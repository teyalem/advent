open Advent

(* Simple predicates *)
let is_num c = '0' <= c && c <= '9'
let is_whitespace c = c = ' '

let char_to_num c = Char.(code c - code '0')

module Expr = struct
  type t = Empty
         | Num of int
         | Add of t * t
         | Mul of t * t

  let rec print = function
    | Empty -> ()
    | Num n -> Printf.printf " %d" n
    | Add (l, r) -> print_string " +"; print l; print r
    | Mul (l, r) -> print_string " *"; print l; print r

  exception Parse_error of string

  let parse_part1 str =
    let construct op lhs rhs = match op with
      | '+' -> Add (lhs, rhs)
      | '*' -> Mul (lhs, rhs)
      | _ -> raise (Parse_error "construct")
    in

    let rec parse_num_or_paren seq =
      match seq () with
      | Seq.Nil -> raise (Parse_error "parse_num")
      | Seq.Cons (c, seq) ->
        if c = '('
        then
          let lhs, seq = parse_num_or_paren seq in
          parse_expr lhs seq
        else Num (char_to_num c), seq

    and parse_expr tmp seq =
      match seq () with
      | Seq.Nil -> tmp, Seq.empty
      | Seq.Cons (c, seq) -> begin
          match c with
          | '+' | '*' as op ->
            let rhs, seq = parse_num_or_paren seq in
            let expr = construct op tmp rhs in
            parse_expr expr seq

          | ')' -> tmp, seq

          | _ -> raise (Parse_error "parse_expr")
        end
    in

    let seq = String.to_seq str
              |> Seq.filter (fun c -> not (is_whitespace c))
    in
    let n, seq = parse_num_or_paren seq in
    parse_expr n seq |> fst

  (* LL parser *)
  let parse_part2 str =
    let rec parse_np = function
      | [] -> raise (Parse_error "parse_np")
      | '(' :: tl -> parse_mul tl
      | n :: tl -> (Num (char_to_num n)), tl

    and parse_add tl =
      let rec loop lhs = function
        | [] -> lhs, []
        | ('*'::_) as tl -> lhs, tl
        | (')'::_) as tl -> lhs, tl
        | '+' :: tl ->
          let rhs, tl = parse_np tl in
          loop (Add (lhs, rhs)) tl
        | _ -> raise (Parse_error "parse_add")
      in
      let lhs, tl = parse_np tl in
      loop lhs tl

    and parse_mul tl =
      let rec loop lhs = function
        | [] -> lhs, []
        | ')' :: tl -> lhs, tl
        | '*' :: tl ->
          let rhs, tl = parse_add tl in
          loop (Mul (lhs, rhs)) tl
        | l -> List.iter print_char l; raise (Parse_error "parse_mul")
      in
      let lhs, tl = parse_add tl in
      loop lhs tl
    in

    let tl = String.to_seq str
             |> Seq.filter (fun c -> not (is_whitespace c))
             |> List.of_seq
    in
    parse_mul tl |> fst

  let rec calculate = function
    | Empty -> 0
    | Num n -> n
    | Add (l, r) -> calculate l + calculate r
    | Mul (l, r) -> calculate l * calculate r

end

let main path =
  let data = open_in path |> IO.read_lines in
  begin
    (* PART 1 *)
    data
    |> List.map Expr.parse_part1
    |> List.map Expr.calculate
    |> sum
    |> print_int;

    print_newline ();

    (* PART 2 *)
    data
    |> List.map Expr.parse_part2
    |> List.map Expr.calculate
    |> sum
    |> print_int;
  end

let _ = Arg.parse [] main ""
