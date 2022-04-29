open Ut

let rec cancel_char seq () =
  match seq () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (c, seq) ->
    if c = '!'
    then cancel_char (Seq.drop 1 seq) ()
    else Seq.Cons(c, cancel_char seq)

let rec remove_garbage seq () =
  match seq () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (c, seq) ->
    if c = '<' then
      let seq = Seq.drop_while ((<>) '>') seq |> Seq.drop 1 in
      remove_garbage seq ()
    else Seq.Cons (c, remove_garbage seq)

let count_score seq =
  let rec aux acc lv seq =
    match seq () with
    | Seq.Nil -> acc
    | Seq.Cons(c, seq) ->
      if c = '{' then aux acc (lv+1) seq
      else if c = '}' then aux (acc+lv) (lv-1) seq
      else aux acc lv seq
  in
  aux 0 0 seq

let rec collect_garbage seq () =
  match seq () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (c, seq) ->
    if c = '<' then
      let gar = Seq.take_while ((<>) '>') seq in
      let seq = Seq.drop_while ((<>) '>') seq |> Seq.drop 1 in
      Seq.Cons (gar, collect_garbage seq)
    else
      collect_garbage seq ()

let () =
  let data = IO.read_all () |> String.to_seq |> cancel_char in
  (* PART 1 *)
  data
  |> remove_garbage
  |> count_score
  |> Printf.printf "%d\n";

  (* PART 2 *)
  data
  |> collect_garbage
  |> Seq.concat
  |> Seq.length
  |> Printf.printf "%d\n";
