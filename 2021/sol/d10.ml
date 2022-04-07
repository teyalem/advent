open Ut
open List

let matching_pair = [ '(', ')'; '[', ']'; '<', '>'; '{', '}'; ]

let is_close c =
  map snd matching_pair |> exists ((=) c)

let is_pair o c =
  assoc o matching_pair = c

let points_illegal = function
  | ')' -> 3
  | ']' -> 57
  | '}' -> 1197
  | '>' -> 25137
  | _ -> assert false

let paren_value = function
  | ')' -> 1
  | ']' -> 2
  | '}' -> 3
  | '>' -> 4
  | _ -> assert false

let points_missing cs =
  List.map paren_value cs
  |> List.fold_left (fun p n -> 5*p + n) 0

let process seq =
  (* good ol' stack machine *)
  let rec aux stack seq =
    match seq () with
    | Seq.Nil ->
      let missing = map (Fun.flip assoc matching_pair) stack in
      Either.Right missing
    | Seq.Cons (c, seq) ->
      if is_close c then
        if is_pair (hd stack) c
        then aux (tl stack) seq
        else Either.Left c (* corrupted *)
      else
        aux (c::stack) seq
  in
  aux [] seq

let () =
  let data = IO.read_lines () |> map String.to_seq in
  let illegal, missing = partition_map process data in
  begin
    (* PART 1 *)
    illegal
    |> map points_illegal
    |> sum
    |> Printf.printf "%d\n";

    (* PART 2 *)
    missing
    |> map points_missing
    |> sort Int.compare
    |> (fun ns -> nth ns @@ length ns / 2)
    |> Printf.printf "%d\n";
  end
