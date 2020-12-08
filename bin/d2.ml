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
  in not (ca = cb)

(* wrapper *)
let is_valid pol pass = pol pass

let parse_policy_pass rule line =
    Scanf.sscanf line "%d-%d %c: %s"
      (fun a b ch pass -> (rule a b ch), pass)

let read_policy_pass rule file =
  Util.read_lines_from file
  |> List.map (parse_policy_pass rule)

let main path =
  let file = open_in path in
  read_policy_pass rule_b file
  |> List.fold_left (fun c (pol, pass) ->
      c + if is_valid pol pass then 1 else 0) 0
  |> print_int
 
let _ = Arg.parse [] main ""
