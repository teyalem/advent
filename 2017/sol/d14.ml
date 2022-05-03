open Ut
open Knot_hash

let rows str =
  Seq.ints 0
  |> Seq.map (fun d -> str ^ "-" ^ string_of_int d)
  |> Seq.take 128

let popcount n =
  let rec aux n =
    if n = 0 then 0 else n land 1 + (aux @@ n lsr 1)
  in
  aux n

let to_bins arr =
  let rec aux i n =
    if i = 8 then []
    else (n land 1) :: aux (i+1) (n lsr 1)
  in
  Array.to_seq arr
  |> Seq.concat_map (fun n -> List.(aux 0 n |> rev |> to_seq))

let count_region disk =
  let disk = Seq.map to_bins disk |> Mat.of_seq in
  let dx = Mat.dimx disk and dy = Mat.dimy disk in
  let visited = Mat.(make dx dy false) in
  let nregions = ref 0 in

  let rec fill (x, y as pos) =
    if not visited.(x).(y) then begin
      visited.(x).(y) <- true;
      Neigh.(neighbors von_neumann pos)
      |> List.filter (fun (x, y) -> 0 <= x && x < dx && 0 <= y && y < dy)
      |> List.filter (fun (x, y) -> disk.(x).(y) = 1)
      |> List.iter fill
    end
  in

  let find_newpoint () =
    let pos = ref (0, 0) in
    try Mat.iteri (fun x y v ->
        if v = 1 && visited.(x).(y) = false then
          begin pos := (x, y); raise Exit end)
        disk;
      -1, -1
    with Exit -> !pos
  in

  let rec aux () =
    let pos = find_newpoint () in
    if pos = (-1, -1) then !nregions
    else begin incr nregions; fill pos; aux () end
  in
  aux ()

let key = "oundnydw"

let () =
  let disk = rows key |> Seq.map knot_hash in
  (* PART 1 *)
  disk
  |> Seq.concat_map (fun hash ->
      hash |> Array.map popcount |> Array.to_seq)
  |> Seq.fold_left Int.add 0
  |> Printf.printf "%d\n";

  (* PART 2 *)
  count_region disk
  |> Printf.printf "%d\n";
