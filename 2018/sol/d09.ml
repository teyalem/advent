let of_list xs =
  [], xs

let add n (l, r) =
  l, n::r

let remove (l, r) =
  match r with
  | [] -> let r = List.rev l in List.(hd r, ([], tl r))
  | x::xs -> x, (l, xs)

let rotate n (l, r) =
  let rec aux n l r =
    if n = 0 then l, r
    else match r with
      | [] -> aux n [] (List.rev l)
      | x::xs -> aux (n-1) (x::l) xs
  in
  if n < 0
  then let r, l = aux ~-n r l in l, r
  else aux n l r

let step (slen, scores, ring, player) marble =
  if marble mod 23 = 0 then begin
    let n, ring = rotate ~-7 ring |> remove in
    scores.(player) <- scores.(player) + marble + n;
    slen, scores, ring, (player + 1) mod slen
  end
  else
    let ring = rotate 2 ring |> add marble in
    slen, scores, ring, (player + 1) mod slen

let game_point nplayers max_point =
  let t = nplayers, Array.make nplayers 0, of_list [0], 0 in
  Seq.init max_point succ
  |> Seq.fold_left step t
  |> (fun (_, scores, _, _) ->
      Array.fold_left max min_int scores)

let parse str =
  Scanf.sscanf str "%d players; last marble is worth %d points" (fun np mp -> np, mp)

let () =
(*  let data = read_line () |> parse in *)
  let nplayers, max_point = read_line () |> parse in
  let f = game_point nplayers in
  (* PART 1 *) f max_point |> print_int;
  print_newline ();
  (* PART 2 *) f (max_point*100) |> print_int;
