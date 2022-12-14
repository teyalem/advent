open Ut

let parse str =
  Str.(split (regexp "->") str)
  |> List.map (fun s -> Scanf.sscanf s " %d,%d " (fun x y -> x, y))

let signum n =
  if n > 0 then 1
  else if n = 0 then 0
  else -1

let add (x, y) (a, b) =
  x+a, y+b

let delta_unit (x, y) (a, b) =
  signum (a-x), signum (b-y)

let draw map posl =
  let rec aux delta (x, y as pos) dest =
    map.(x).(y) <- '#';
    if pos <> dest then
      aux delta (add pos delta) dest
  in
  match posl with
  | x::xs ->
    List.fold_left
      (fun p d -> aux (delta_unit p d) p d; d)
      x xs
    |> ignore
  | _ -> assert false

let gen_map posll =
  let xs, ys = List.concat posll |> List.split in
  let xmax = List.fold_left max min_int xs
  and ymax = List.fold_left max min_int ys in
  let map = Array.make_matrix (xmax*2) (ymax+2) '.' in
  List.iter (draw map) posll;
  map

let move_until_rest ?(part2 = false) map =
  let dimy = Array.length map.(0) in
  let rec aux (x, y) =
    if y + 1 = dimy then x, y
    else if map.(x).(y+1) = '.' then aux (x, y+1)
    else if map.(x-1).(y+1) = '.' then aux (x-1, y+1)
    else if map.(x+1).(y+1) = '.' then aux (x+1, y+1)
    else x, y
  in
  let x, y = aux (500, 0) in
  if not part2 && y + 1 = dimy then false
  else if part2 && (x, y) = (500, 0) then false
  else begin map.(x).(y) <- 'o'; true end

let solve1 map =
  let c = ref 0 in
  while move_until_rest map do
    incr c
  done;
  !c

let solve2 map =
  let c = ref 0 in
  while move_until_rest ~part2: true map do
    incr c
  done;
  !c + 1

let () =
  let data = open_in Sys.argv.(1) |> IO.input_lines |> List.map parse in
  let map = gen_map data in solve1 map |> print_int;
  print_newline ();
  let map = gen_map data in solve2 map |> print_int
