open Ut

module M = Map.Make(String)
module S = Set.Make(String)

type graph = (string * int) list M.t

let to_graph xs =
  let add f t d m =
    match M.find_opt f m with
    | None -> M.add f [t, d] m
    | Some l -> M.add f ((t, d) :: l) m
  in
  List.fold_left
    (fun m (f, t, d) -> m |> add f t d |> add t f d)
    M.empty xs

let collect_cities g =
  M.to_seq g |> Seq.map fst |> S.of_seq

let rec insert x = function
  | [] -> [[x]]
  | y::xs ->
    (x::y::xs) :: List.map (fun l -> y::l) (insert x xs)

let rec combinations = function
  | [] -> [[]]
  | x::xs -> List.concat_map (insert x) @@ combinations xs

let calc_cost g r =
  let open List in
  fold_left (fun (w, p) c ->
      let w = w + (M.find p g |> assoc c) in w, c)
    (0, hd r) (tl r)
  |> fst

let dists g =
  let all_cities = collect_cities g in
  let combis = combinations @@ S.elements all_cities in
  List.map (calc_cost g) combis

let parse str =
  Scanf.sscanf str "%s to %s = %d" (fun f t d -> f, t, d)

let () =
  let open List in
  let data = IO.read_lines () |> map parse |> to_graph in
  let ds = dists data |> sort Int.compare in
  begin
    (* PART 1 *) hd ds |> Printf.printf "%d\n";
    (* PART 2 *) nth ds (length ds - 1) |> Printf.printf "%d\n";
  end
