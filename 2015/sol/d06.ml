open Ut

type range = int * int * int * int

type com =
  | On
  | Off
  | Toggle

let parse str =
  let f com a b c d = com, (a, b, c, d) in
  if String.starts_with ~prefix: "toggle" str then
    Scanf.sscanf str "toggle %d,%d through %d,%d" (f Toggle)
  else
    Scanf.sscanf str "turn %s %d,%d through %d,%d"
      (fun com -> if com = "on" then f On else f Off)

let apply mat f (sx, sy, ex, ey) =
    for x = sx to ex do
      for y = sy to ey do
        let n = Mat.get mat x y in
        Mat.set mat x y @@ f n
      done
    done

let lit_part1 coms =
  let mat = Mat.make 1000 1000 false in
  let f c n =
    match c with
    | On -> true
    | Off -> false
    | Toggle -> not n
  in
  coms |> List.iter (fun (c, r) -> apply mat (f c) r);
  Mat.to_seq mat
  |> Seq.concat
  |> Seq.fold_left (fun p e -> p + if e then 1 else 0) 0

let lit_part2 coms =
  let mat = Mat.make 1000 1000 0 in
  let f c n =
    match c with
    | On -> n+1
    | Off -> max 0 (n-1)
    | Toggle -> n+2
  in
  coms |> List.iter (fun (c, r) -> apply mat (f c) r);
  Mat.to_seq mat
  |> Seq.concat
  |> Seq.fold_left Int.add 0

let () =
  let data = IO.read_lines () |> List.map parse in
  begin
    (* PART 1 *) lit_part1 data |> Printf.printf "%d\n";
    (* PART 2 *) lit_part2 data |> Printf.printf "%d\n";
  end
