open Ut

(* Policy is a simple validation function.
 * Rule generates policy. *)
type policy = string -> bool
type rule = int -> int -> char -> policy

let parse_policy_pass rule line =
    Scanf.sscanf line "%d-%d %c: %s"
      (fun a b ch pass -> (rule a b ch), pass)

(* Part 1 *)
let rule_a s e ch = fun pass ->
  let index = String.index pass ch
  and range = Range.make s e
  in Range.contains range index

(* Part 2 *)
let rule_b a b ch = fun pass ->
  let ca = pass.[a-1] = ch
  and cb = pass.[b-1] = ch
  in ca <> cb

(* count valid passwords *)
let count_valid l =
  List.filter (fun (pol, pass) -> pol pass) l
  |> List.length

let main path =
  let data = open_in path |> IO.input_lines in
  let make_list rule = List.map (parse_policy_pass rule) data in
  begin
    (* PART 1 *)
    make_list rule_a
    |> count_valid
    |> print_int;

    print_newline ();

    (* PART 2 *)
    make_list rule_b
    |> count_valid
    |> print_int

  end

let _ = Arg.parse [] main ""
