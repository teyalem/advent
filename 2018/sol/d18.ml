let get mat x y =
  try mat.(y).(x)
  with Invalid_argument _ -> '?'

let set mat x y c =
  try mat.(y).(x) <- c
  with Invalid_argument _  -> begin
      Printf.eprintf "%d %d\n" x y;
      failwith "out of bounds"
    end

let count =
  Seq.fold_left (fun (o, t, l) -> function
      | '.' -> o+1, t, l
      | '|' -> o, t+1, l
      | '#' -> o, t, l+1
      | '?' -> o, t, l
      | _ -> assert false)
    (0, 0, 0)

let next neigh =
  let _, t, l = count neigh in
  function
  | '.' -> if t >= 3 then '|' else '.'
  | '|' -> if l >= 3 then '#' else '|'
  | '#' -> if l >= 1 && t >= 1 then '#' else '.'
  | _ -> assert false

let n8 = [
  -1, -1; 0, -1; 1, -1; -1, 0; 1, 0; -1, 1; 0, 1; 1, 1
] |> List.to_seq

let neighbors mat x y =
  Seq.map (fun (dx, dy) -> get mat (x+dx) (y+dy)) n8

let cellauto mat =
  let updates = ref [] in
  mat |> Array.iteri (fun y -> Array.iteri (fun x c ->
      let n = next (neighbors mat x y) c in
      if n <> c then updates := (x, y, n) :: !updates));
  !updates |> List.iter (fun (x, y, c) -> set mat x y c);
  mat

let copy =
  Array.(map copy)

let after n mat =
  let rec aux i m =
    if i = n then m
    else aux (i+1) @@ cellauto m
  in
  aux 0 @@ copy mat

let resource_value mat =
  mat
  |> Array.to_seq
  |> Seq.map Array.to_seq
  |> Seq.concat
  |> count
  |> (fun (_, t, l) -> t * l)


let solve1 mat =
  resource_value @@ after 10 mat

let solve2 mat =
  let rec aux prevs i m =
    match List.find_opt (fun (_, a) -> a = m) prevs with
    | None -> aux ((i, m) :: prevs) (i+1) @@ cellauto @@ copy m
    | Some (p, _) ->
      let t = ((1_000_000_000 - p) mod (i - p)) + p in
      List.assoc t prevs
  in
  resource_value @@ aux [] 0 mat

let () =
  let data =
    open_in Sys.argv.(1)
    |> In_channel.input_all
    |> String.trim
    |> String.split_on_char '\n'
    |> List.map (fun s -> String.to_seq s |> Array.of_seq)
    |> Array.of_list
  in
  (* PART 1 *) solve1 data |> print_int;
  print_newline ();
  (* PART 2 *) solve2 data |> print_int;
