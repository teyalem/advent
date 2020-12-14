type range = int * int

(* test *)
let contains (s, e) n = s <= n && n <= e

type policy = string -> bool
type rule = int -> int -> char -> policy

(* Part 1 *)
let rule_a s e ch = fun pass ->
  let count = String.to_seq pass
              |> Seq.filter ((=) ch)
              |> Seq.fold_left (fun a _ -> a+1) 0
  and limit = (s, e)
  in contains limit count

(* Part 2 *)
let rule_b a b ch = fun pass ->
  let ca = pass.[a-1] = ch
  and cb = pass.[b-1] = ch
  in ca <> cb

let parse_policy_pass rule line =
    Scanf.sscanf line "%d-%d %c: %s"
      (fun a b ch pass -> (rule a b ch), pass)

let main path =
  let data = open_in path |> Util.read_lines in
  let make_list rule = List.map (parse_policy_pass rule) data in
  begin
    (* PART 1 *)
    make_list rule_a
    |> List.fold_left
      (fun c (pol, pass) -> c + if pol pass then 1 else 0)
      0
    |> print_int;

    print_newline ();

    (* PART 2 *)
    make_list rule_b
    |> List.fold_left
      (fun c (pol, pass) -> c + if pol pass then 1 else 0)
      0
    |> print_int
  end

let _ = Arg.parse [] main ""
