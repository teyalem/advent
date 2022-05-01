open Ut

module M = Map.Make(Int)
module S = Set.Make(Int)

let parse str =
  Scanf.sscanf str "%d <-> %s@!" (fun n nei ->
      n,
      String.split_on_char ',' nei
      |> List.map String.trim
      |> List.map int_of_string
      |> S.of_list)

(* This collects nodes that contains at least one element of the set, leaving uncollected nodes
 * aside. Think of gathering fruits that is connected at some branches. *)
let divide set =
  List.partition_map (fun (n, nei as node) ->
        if not (S.mem n set || S.disjoint nei set)
        then Left n
        else Right node)

let rec collect graph set =
  let ns, rest = divide set graph in
  if ns = [] then set, rest
  else collect rest @@ S.(union set @@ of_list ns)

let find_group_0 graph =
  collect graph @@ S.singleton 0
  |> fst

let collect_groups graph =
  let select = function
    | [] -> assert false
    | (n, _) :: rest -> n, rest
  in

  let rec aux graph sets =
    let n, rest = select graph in
    let set, rest = collect rest @@ S.singleton n in
    let sets = set :: sets in
    if rest = [] then sets else aux rest sets
  in
  aux graph []

let () =
  let data = IO.read_lines () |> List.map parse in
  (* PART 1 *)
  find_group_0 data
  |> S.cardinal
  |> Printf.printf "%d\n";

  (* PART 2 *)
  collect_groups data
  |> List.length
  |> Printf.printf "%d\n";
