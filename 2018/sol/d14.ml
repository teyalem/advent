let round = 509671

let digits n =
  let rec aux n =
    if n = 0 then []
    else n mod 10 :: aux (n / 10)
  in
  if n = 0 then [0] else aux n |> List.rev

type t = int * int * int * int array

let make n : t =
  let arr = Array.make n 0 in
  arr.(0) <- 3; arr.(1) <- 7;
  0, 1, 2, arr

let move a n len =
  (a + n + 1) mod len

let step (a, b, len, arr) =
  let an = arr.(a) and bn = arr.(b) in
  let ds = digits (an + bn) in
  List.iteri (fun i n -> arr.(len + i) <- n) ds;
  assert (let l = List.length ds in 0 < l && l <= 2);
  let len = len + List.length ds in
  move a an len, move b bn len, len, arr

let part1 =
  let t = make @@ round + 11 in
  let rec aux (_, _, l, arr as t) =
    if l >= round + 10
    then Array.sub arr round 10
    else aux @@ step t
  in
  aux t

let part2 =
  let t = make 30_000_001 in (* let's assume sequence appears within that... *)
  let rec fill (_, _, len, arr as t) =
    if len >= 30_000_000 then arr
    else fill @@ step t
  in
  let arr = fill t in
  let seq = digits round |> Array.of_list in
  let rec aux i =
    if Array.sub arr i 6 = seq then i
    else aux @@ i + 1
  in
  aux 0

let () =
  (* PART 1 *) part1 |> Array.iter print_int;
  print_newline ();
  (* PART 2 *) part2 |> print_int;
