open Ut

let is_whitespace = function
  | ' ' | '\t' | '\n' -> true
  | _ -> false

let split_while f seq =
  let x = Seq.take_while f seq
  and seq = Seq.drop_while f seq in
  x, seq

let rec dc_length version seq =
  let rec aux seq = fun () ->
    match seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (x, seq) ->
      if x = '(' then
        let l, seq = split_while ((<>) 'x') seq in
        let t, seq = split_while ((<>) ')') @@ Seq.drop 1 seq in
        let f s = int_of_string @@ String.of_seq s in
        let l = f l and t = f t in
        let il =
          if version = 1 then l
          else if version = 2 then
            dc_length 2 @@ Seq.take l @@ Seq.drop 1 seq
          else assert false
        in
        Seq.Cons (il*t, aux @@ Seq.drop (1+l) seq)
      else
        Seq.Cons (1, aux seq)
  in
  aux seq |> Seq.fold_left Int.add 0

let () =
  let data = IO.read_all () |> String.to_seq in
  begin
    (* PART 1 *)
    dc_length 1 data |> Printf.printf "%d\n";

    (* PART 2 *)
    dc_length 2 data |> Printf.printf "%d\n";
  end
