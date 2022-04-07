open Ut

let rec collatz n =
  if n = 1 then 0
  else if n mod 2 = 0
  then 1 + collatz (n/2)
  else 1 + collatz (3*n+1)

let accumulate init =
  Seq.fold_left (fun acc c ->
      if String.starts_with ~prefix: "tpl" c then
        3*acc
      else if String.starts_with ~prefix: "inc" c then
        acc+1
      else assert false)
    init

let calc_a_part1 code =
  code
  |> Seq.drop 1
  |> Seq.take_while (Fun.negate @@ String.starts_with ~prefix: "jmp")
  |> accumulate 0

let calc_a_part2 code =
  let n, code = Seq.uncons code |> Option.get in
  let n = Scanf.sscanf n "jio a, +%d" Fun.id in
  code
  |> Seq.drop (n-1) (* cuz we dropped first line of code *)
  |> Seq.take_while (Fun.negate @@ String.starts_with ~prefix: "jio")
  |> accumulate 1

let () =
  let code = IO.read_lines () |> List.to_seq in
  begin
    (* PART 1 *)
    calc_a_part1 code |> collatz |> Printf.printf "%d\n";

    (* PART 2 *)
    calc_a_part2 code |> collatz |> Printf.printf "%d\n";
  end
