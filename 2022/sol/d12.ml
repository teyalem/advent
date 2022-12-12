open Ut

let parse sl =
  let s = ref (0, 0) and e = ref (0, 0) in
  let m =
    sl |> List.mapi (fun y l ->
        l
        |> String.to_seq
        |> List.of_seq
        |> List.mapi (fun x c ->
            if c = 'S' then begin s := (x, y); 0 end
            else if c = 'E' then begin e := (x, y); 25 end
            else if 'a' <= c && c <= 'z' then Char.(code c - code 'a')
            else assert false)
        |> Array.of_list
      )
    |> Array.of_list
  in
  !s, !e, m

let neigh (x, y) =
  [ -1, 0; 1, 0; 0, -1; 0, 1 ]
  |> List.map (fun (dx, dy) -> x+dx, y+dy)

let find_path s e map =
  let xl = Array.length map.(0)
  and yl = Array.length map in
  let q = Queue.create () in 
  let visited = Array.make_matrix yl xl false in
  let rec aux () =
    if Queue.is_empty q then failwith "No path found"
    else
      let (x, y), step = Queue.take q in
      if (x, y) = e then step
      else if not visited.(y).(x) then begin
        let h = map.(y).(x) in
        visited.(y).(x) <- true;
        neigh (x, y)
        |> List.filter (fun (x, y) -> 0 <= x && x < xl && 0 <= y && y < yl)
        |> List.filter (fun (x, y) -> map.(y).(x) <= h+1)
        |> List.iter (fun pos -> Queue.add (pos, step+1) q);
        aux ()
      end
      else aux ()
  in
  Queue.add (s, 0) q;
  aux ()

let find_a map =
  let xs = ref [] in
  map |> Array.iteri (fun y arr ->
      arr |> Array.iteri (fun x h ->
          if h = 0 then xs := (x, y) :: !xs));
  !xs

let () =
  let s, e, map = open_in Sys.argv.(1) |> IO.input_lines |> parse in
  find_path s e map |> print_int;
  print_newline ();

  find_a map
  |> List.map (fun pos -> try find_path pos e map with Failure _ -> max_int)
  |> List.fold_left min max_int
  |> print_int
