open Ut

let parse str =
  Scanf.sscanf str "/dev/grid/node-x%d-y%d %dT %dT %dT %d%%"
    (fun x y _ used avail _ ->
       (x, y), (used, avail))

let place_nodes seq =
  let hx, hy = Seq.map fst seq |> Seq.fold_left max (0, 0) in
  let mat = Mat.make (hx+1) (hy+1) (0, 0) in
  seq |> Seq.iter (fun ((x, y), v) -> Mat.set mat x y v);
  mat

let n_viable_pairs mat =
  Mat.to_seq mat
  |> Seq.concat
  |> Seq.filter (fun (u, a) -> not (u = 0 || (a < u && a+u > 100)))
  |> Seq.length

let print mat =
  Mat.iter_row (fun (u, a) ->
      Printf.printf "%c" @@
      if u = 0 then '_'
      else if a < u && u+a > 100 then '#'
      else '.')
    (fun () -> Printf.printf "\n")
    mat

module M = struct
  type space = char Mat.t
  type state = int * int
  type data = unit
  type weight = int

  let data_id = ()

  let is_end m (p, _) =
    p = (Mat.dimx m - 2, 0)

  let neighbors m (p, _) =
    Neigh.(neighbors von_neumann p)
    |> List.filter (fun (x, y) ->
        0 <= x && x < Mat.dimx m && 0 <= y && y < Mat.dimy m)
    |> List.filter (fun (x, y) -> Mat.get m x y <> '#')
    |> List.map (fun p -> 1, p, ())

end

let solve mat =
  let module H = struct
    type t = int * int
    let compare = Stdlib.compare
  end
  in

  let map =
    Mat.map (fun (u, a) ->
        if u = 0 then '_' else
        if a < u && u+a > 100 then '#'
        else '.')
      mat
  in
  let empty = ref (0, 0) in
  Mat.iteri (fun x y c -> if c = '_' then empty := (x, y)) map;
  let es =
    Pathfind.dijkstra (module H) (module Int) (module M) ~start: !empty map
    |> fst
  in
  es + 1 + 5 * (Mat.dimx map - 2)

let () =
  let data =
    IO.read_lines ()
    |> List.to_seq
    |> Seq.drop 2
    |> Seq.map parse
    |> Seq.memoize
  in
  let mat = place_nodes data in

  (* PART 1 *)
  n_viable_pairs mat |> Printf.printf "%d\n";

  (* PART 2 *)
  solve mat |> Printf.printf "%d\n";
