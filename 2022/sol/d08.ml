open Ut

let read_map ss =
  ss
  |> List.map (fun s ->
      String.to_seq s
      |> Seq.map (fun c -> Char.(code c - code '0'))
      |> Array.of_seq)
  |> Array.of_list

let see_array map =
  let dimx = Array.length map.(0)
  and dimy = Array.length map in
  let out = Array.make_matrix dimy dimx false in
  let rec aux (dx, dy) (x, y) m =
    if 0 <= x && x < dimx && 0 <= y && y < dimy then begin
      let v = map.(y).(x) in
      if v > m then out.(y).(x) <- true;
      let m = max v m in
      aux (dx, dy) (x+dx, y+dy) m
    end
  in
  let f d p = aux d p min_int in
  List.init dimx Fun.id
  |> List.iter (fun x ->
      f (0, 1) (x, 0);
      f (0, -1) (x, dimy-1));
  List.init dimy Fun.id
  |> List.iter (fun y ->
      f (1, 0) (0, y);
      f (-1, 0) (dimx-1, y));
  out

let scenic_scores map =
  let dimx = Array.length map.(0)
  and dimy = Array.length map in
  let out = Array.make_matrix dimy dimx 0 in
  let rec aux (dx, dy) (x, y) h count =
    if 0 <= x && x < dimx && 0 <= y && y < dimy then begin
      let v = map.(y).(x) in
      if v < h
      then aux (dx, dy) (x+dx, y+dy) h (count+1)
      else count + 1
    end
    else count
  in
  let f (x, y) (dx, dy) = aux (dx, dy) (x+dx, y+dy) map.(y).(x) 0 in
  for y = 0 to dimy-1 do
    for x = 0 to dimx-1 do
      [ -1, 0; 1, 0; 0, -1; 0, 1 ]
      |> List.map (f (x, y))
      |> List.fold_left Int.mul 1
      |> (fun s -> out.(y).(x) <- s)
    done
  done;
  out

let flat_mat m =
  Array.to_seq m
  |> Seq.map Array.to_seq
  |> Seq.concat

let () =
  let data = open_in Sys.argv.(1) |> IO.input_lines |> read_map in
  see_array data
  |> flat_mat
  |> Seq.fold_left
    (fun acc n -> if n then acc + 1 else acc)
    0
  |> print_int;
  print_newline ();

  scenic_scores data
  |> flat_mat
  |> Seq.fold_left max min_int
  |> print_int
