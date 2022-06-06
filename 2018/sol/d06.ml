open Ut

module S = Set.Make(Int)
module M = Map.Make(Int)

let parse str =
  Scanf.sscanf str "%d, %d" (fun a b -> a, b)

let manhattan (a, b) (c, d) =
  abs (c - a) + abs (d - b)

let closest ps pos =
  let ds =
    List.mapi (fun i v -> i, manhattan pos v) ps
    |> List.sort (fun (_, n) (_, m) -> Int.compare n m)
  in
  let min_d = List.hd ds |> snd in
  let mins = List.filter (fun (_, d) -> d = min_d) ds in
  if List.length mins > 1 then -1 else List.hd ds |> fst

let paint ps =
  Seq.init 1000 (fun x ->
      Seq.init 1000 (fun y -> (x, y), closest ps (x, y)))
  |> Seq.concat
  |> Seq.memoize

let find_infs mat =
  Seq.filter (fun ((x, y), _) -> x = 0 || y = 0 || x = 999 || y = 999) mat
  |> Seq.map snd
  |> S.of_seq

let collect mat =
  let inc map k =
    M.update k (function None -> Some 1 | Some n -> Some (n+1)) map
  in
  Seq.map snd mat
  |> Seq.fold_left inc M.empty

let find_largest_noninf_area infs areas =
  M.filter (fun k _ -> k <> -1 && not @@ S.mem k infs) areas
  |> M.to_seq
  |> Seq.fold_left (fun (_, pc as p) (_, nc as n) ->
      if nc > pc then n else p)
    (-2, min_int)

let sum_dist ps pos =
  List.map (manhattan pos) ps
  |> List.fold_left Int.add 0

let safe_area ps =
  Seq.init 1000 (fun x ->
      Seq.init 1000 (fun y -> sum_dist ps (x, y)))
  |> Seq.concat
  |> Seq.filter (fun d -> d < 10000)

let () =
  let data = IO.read_lines () |> List.map parse in
  (* PART 1 *)
  let mat = paint data in
  let infs = find_infs mat and areas = collect mat in
  find_largest_noninf_area infs areas |> snd |> print_int;
  print_newline ();

  (* PART 2 *)
  safe_area data |> Seq.length |> print_int;
