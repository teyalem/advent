module P = struct
  type t = int list
  let compare = List.compare Int.compare
end

module S = Set.Make(P)

let distance n m =
  List.map2 (fun a b -> abs (a - b)) n m
  |> List.fold_left Int.add 0

let rec pull_connected s graph =
  let nei, rest = List.partition (fun (v, _) -> S.mem v s) graph in
  if nei = [] then s, rest
  else
    let s = List.(fold_left S.union s @@ map snd nei) in
     pull_connected s rest

let split_constellation graph =
  let rec aux sets = function
    | [] -> sets
    | (v, n) :: xs ->
      let s, rest = pull_connected (S.add v n) xs in
      aux (s::sets) rest
  in
  aux [] graph

let build_graph points =
  List.map (fun p ->
      let nei = List.filter (fun v -> distance p v <= 3) points |> S.of_list in
      p, nei)
    points

let parse str =
  Scanf.sscanf str "%d,%d,%d,%d" (fun x y z w -> [x; y; z; w])

let () =
  let data =
    open_in Sys.argv.(1)
    |> In_channel.input_all
    |> String.trim
    |> String.split_on_char '\n'
    |> List.map parse
  in
  (* PART 1 *)
  data |> build_graph |> split_constellation |> List.length |> print_int
