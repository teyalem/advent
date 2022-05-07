let step n (arr, p) m =
  let len = Array.length arr in
  let np = (p + n) mod len + 1 in
  Array.init (len + 1) (fun i ->
      if i < np then arr.(i)
      else if i = np then m
      else arr.(i-1)),
  np

let step2 n (nz, len, p) m =
  let np = (p + n) mod len + 1 in
  (if np = 1 then m else nz), len+1, np

let input = 335

let () =
  Seq.init 2017 succ
  |> Seq.fold_left (step input) ([|0|], 0)
  |> (fun (arr, i) -> arr.((i+1) mod 2018))
  |> print_int;
  print_newline ();

  let fifty_million = 50_000_000 in
  Seq.init fifty_million succ
  |> Seq.fold_left (step2 input) (0, 1, 0)
  |> (fun (nz, _,_) -> nz)
  |> print_int
