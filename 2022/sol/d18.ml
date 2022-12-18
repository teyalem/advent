open Ut

module P3 = struct
  type t = int * int * int

  let compare (x, y, z) (u, v, w) =
    match Int.compare x u with
    | 0 ->
      (match Int.compare y v with
       | 0 -> Int.compare z w
       | n -> n)
    | n -> n

  let neighs (x, y, z) =
    [ -1, 0, 0;
      1, 0, 0;
      0, -1, 0;
      0, 1, 0;
      0, 0, -1;
      0, 0, 1
    ] |> List.map (fun (dx, dy, dz) -> x+dx, y+dy, z+dz)

  let parse str =
    Scanf.sscanf str "%d,%d,%d" (fun x y z -> x, y, z)
end

module S = Set.Make(P3)

let surface_area cubes =
  let set = S.of_list cubes in
  cubes
  |> List.map (fun pos ->
      let nei = S.of_list @@ P3.neighs pos in
      S.(cardinal @@ diff nei set))
  |> List.fold_left (+) 0

let fill_interior cubes =
  let set = S.of_list cubes in
  let cover = ref @@ S.of_list cubes in
  let range (x, y, z) =
    let f n = -5 <= n && n <= 25 in
    f x && f y && f z
  in
  let rec aux pos =
    if range pos
    && not (S.mem pos set || S.mem pos !cover)
    then begin
      cover := S.add pos !cover;
      S.(diff (of_list @@ P3.neighs pos) @@ union !cover set)
      |> S.iter aux
    end
  in
  aux (-5, -5, -5);

  cubes
  |> List.map (fun pos ->
      S.(inter !cover @@ diff (of_list @@ P3.neighs pos) set)
      |> S.cardinal)
  |> List.fold_left (+) 0

let () =
  let data = open_in Sys.argv.(1) |> IO.input_lines |> List.map P3.parse in

  surface_area data |> print_int;
  print_newline ();
  fill_interior data |> print_int
