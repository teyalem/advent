open Ut

let rotatel (x, y) = -y, x
let rotater (x, y) = y, -x

let rotate r (x, y) =
  if r = 'L' then rotatel (x, y)
  else if r = 'R' then rotater (x, y)
  else assert false

let parse str =
  Scanf.sscanf str "%c%d" (fun r d -> r, d)

let walk ((x, y), (dx, dy)) (r, d) =
  let dx, dy = rotate r (dx, dy) in
  (x + d*dx, y + d*dy), (dx, dy)

let taxi_distance (x, y) =
  abs x + abs y

let zero = 0, 0
let north = 0, 1

let find_ebhq insts =
  let insts = Seq.cycle @@ List.to_seq insts in
  let ps = Hashtbl.create 100 in
  let rec aux (x, y) (dx, dy) insts =
    match insts () with
    | Seq.Nil -> assert false
    | Seq.Cons ((r, d), insts) ->
      let dx, dy = rotate r (dx, dy) in
      Seq.init d (fun d -> x + d*dx, y + d*dy)
      |> Seq.find_map (fun (x, y) ->
          if Hashtbl.mem ps (x, y)
          then Some (x, y)
          else begin Hashtbl.add ps (x, y) true; None end)
      |> (fun p ->
          if Option.is_none p
          then aux (x + d*dx, y + d*dy) (dx, dy) insts
          else Option.get p)
  in
  aux zero north insts

let () =
  let data =
    IO.read_all ()
    |> String.split_on_char ','
    |> List.map String.trim
    |> List.map parse
  in
  begin
    (* PART 1 *)
    data
    |> List.fold_left walk (zero, north)
    |> fst
    |> taxi_distance
    |> Printf.printf "%d\n";

    (* PART 2 *)
    data
    |> find_ebhq
    |> taxi_distance
    |> Printf.printf "%d\n";
  end
