open Ut

(* Policy is a simple validation function.
 * Rule generates policy. *)
type policy = string -> bool
type rule = int -> int -> char -> policy

let parse_policy_pass rule line =
    Scanf.sscanf line "%d-%d %c: %s"
      (fun a b ch pass -> (rule a b ch), pass)

(* Part 1 *)
let rule1 s e ch = fun pass ->
  let n =
    String.to_seq pass
    |> Seq.filter ((=) ch)
    |> List.of_seq
    |> List.length
  in
  s <= n && n <= e

(* Part 2 *)
let rule2 a b ch = fun pass ->
  let ca = pass.[a-1] = ch
  and cb = pass.[b-1] = ch in
  ca <> cb

(* count valid passwords *)
let count_valid l =
  List.filter (fun (pol, pass) -> pol pass) l
  |> List.length

let () =
  let data = IO.read_lines () in
  let f rule =
    List.map (parse_policy_pass rule) data
    |> count_valid
    |> Printf.printf "%d\n";
  in
  begin
    (* PART 1 *) f rule1;
    (* PART 2 *) f rule2;
  end
