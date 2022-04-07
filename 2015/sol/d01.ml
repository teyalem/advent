open Ut

let dir = function
  | '(' -> 1
  | ')' -> -1
  | _ -> raise (Invalid_argument "dir")

let find_base seq =
  let rec loop i floor seq =
    match seq () with
    | Seq.Nil -> i
    | Seq.Cons (c, f) ->
      let floor = floor + dir c in
      if floor < 0 then i
      else loop (i+1) floor f
  in
  loop 1 0 seq

let () =
  let data = IO.read_all () |> String.to_seq in
  begin
    (* PART 1 *)
    data
    |> Seq.fold_left (fun p n -> p + dir n) 0
    |> Printf.printf "%d\n";

    (* PART 2 *)
    data
    |> find_base
    |> Printf.printf "%d\n";
  end
