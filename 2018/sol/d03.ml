open Ut

let parse str =
  Scanf.sscanf str "#%d %@ %d,%d: %dx%d" (fun id x y w h ->
      id, (x, y, w, h))

let mark mat (x, y, w, h) =
  Seq.init w (fun i -> Seq.init h (fun j -> (x+i, y+j)))
  |> Seq.concat
  |> Seq.iter (fun (x, y) -> mat.(x).(y) <- mat.(x).(y) + 1)

let count mat =
  let cnt = ref 0 in
  Mat.iter (fun n -> if n >= 2 then incr cnt) mat;
  !cnt

let overlap (a, la) (b, lb) =
  let a2 = a + la - 1 and b2 = b + lb - 1 in
  max a b <= min a2 b2

let overlap_claim (_, (x1, y1, w1, h1)) (_, (x2, y2, w2, h2)) =
  overlap (x1, w1) (x2, w2) && overlap (y1, h1) (y2, h2)

let find_nonoverlap xs =
  List.find_opt (fun x ->
      not @@ List.exists (fun y ->
          fst y <> fst x && overlap_claim x y) xs)
    xs

let () =
  let data = IO.read_lines () |> List.map parse in
  (* PART 1 *)
  let arr = Array.make_matrix 1000 1000 0 in
  List.map snd data |> List.iter (mark arr);
  count arr |> print_int;
  print_newline ();

  (* PART 2 *)
  find_nonoverlap data
  |> Option.iter (fun x -> print_int @@ fst x);
