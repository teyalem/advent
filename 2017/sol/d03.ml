open Ut

let up = 0, -1
let down = 0, 1
let left = -1, 0
let right = 1, 0

let add (a, b) (c, d) = a+c, b+d

let spiral_diag =
  let aux = Seq.unfold (fun (n, d) -> Some ((n, d), (n + 4*d, d+2))) in
  aux (1, 2)
  |> Seq.mapi (fun i x -> x, (i, i))

let move n =
  [ up; left; down; right ]
  |> List.to_seq
  |> Seq.concat_map (fun x -> Seq.(repeat x |> take n))
  |> Seq.drop 1
  |> Seq.take (n*4 - 2)
  |> Seq.cons right

let spiral_part m d pos =
  move d
  |> Seq.scan (fun (m, pos) delta -> m+1, add pos delta) (m, pos)

let spiral =
  spiral_diag
  |> Seq.concat_map (fun ((m, d), pos) -> spiral_part m d pos)
  |> Seq.memoize

let to_pos n =
  spiral
  |> Seq.find (fun (m, _) -> m = n)
  |> Option.map snd

let stress n = 
  let tbl = Hashtbl.create 100 in
  let get pos = Hashtbl.find_opt tbl pos in
  let set pos v = Hashtbl.replace tbl pos v in
  let aux pos = (* sum of neighbors *)
    Neigh.(neighbors moore pos)
    |> List.filter_map get
    |> List.fold_left Int.add 0
  in
  set (0, 0) 1;
  spiral
  |> Seq.drop 1 (* prevents overwriting on (0, 0). *)
  |> Seq.map snd
  |> Seq.find_map (fun pos ->
      let m = aux pos in
      if m > n then Some m else begin set pos m; None end)

let taxicab (x, y) = abs x + abs y

let () =
  let n = 361527 in
  (* PART 1 *)
  to_pos n |> Option.get |> taxicab |> Printf.printf "%d\n";
  (* PART 2 *)
  stress n |> Option.iter (Printf.printf "%d\n");
