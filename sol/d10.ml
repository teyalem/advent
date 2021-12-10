open Ut

let matching_pair = [
  '(', ')';
  '[', ']';
  '<', '>';
  '{', '}';
]

let is_close c =
  List.map snd matching_pair
  |> List.exists ((=) c)

let is_pair o c =
  List.assoc o matching_pair = c

let points_illegal = function
  | ')' -> 3
  | ']' -> 57
  | '}' -> 1197
  | '>' -> 25137
  | _ -> assert false

let points_missing = function
  | ')' -> 1
  | ']' -> 2
  | '}' -> 3
  | '>' -> 4
  | _ -> assert false

let calc_points_missing cs =
  List.map points_missing cs
  |> List.fold_left (fun p n -> 5*p + n) 0

let check_corrupted seq =
  let rec aux stack seq =
    match seq () with
    | Seq.Nil -> None
    | Seq.Cons (c, seq) ->
      if is_close c then
        if is_pair (List.hd stack) c then
          aux (List.tl stack) seq
        else
          Some c
      else
        aux (c::stack) seq
  in
  aux [] seq

let check_missing seq = (* only accepts non-corrupted chunks *)
  let rec aux stack seq =
    match seq () with
    | Seq.Nil -> stack
    | Seq.Cons (c, seq) ->
      if is_close c then begin
        if not @@ is_pair (List.hd stack) c then assert false;
        aux (List.tl stack) seq
      end
      else
        aux (c::stack) seq
  in
  aux [] seq
  |> List.map (fun c -> List.assoc c matching_pair)

let () =
  let data = open_in Sys.argv.(1) |> IO.input_lines |> List.map String.to_seq in
  begin
    (* PART 1 *)
    List.filter_map check_corrupted data
    |> List.map points_illegal
    |> sum
    |> Printf.printf "%d\n";

    (* PART 2 *)
    List.filter (fun s -> Option.is_none @@ check_corrupted s) data
    |> List.map check_missing
    |> List.map calc_points_missing
    |> List.sort Int.compare
    |> (fun ns -> List.(nth ns @@ length ns / 2))
    |> Printf.printf "%d\n"
  end
