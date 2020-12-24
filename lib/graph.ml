(** Graph of indexes. *)

open List

(** node and list of adjacent nodes *)
type t = (int * int list) list

(** Print a graph g as '3: 1 2 4' format. *)
let print g =
  iter (fun (i, e) ->
      Printf.printf "%d:" i;
      iter (Printf.printf " %d") e;
      print_newline ()) g

(** Print a graph g in `graphviz` format. It can be used as visualization. *)
let print_graphviz g =
  print_endline "digraph G {";
  iter (fun (i, e) ->
      iter (Printf.printf "%d -> %d\n" i) e) g;
  print_endline "}"

(* check node is connected to other node *)
let is_connected node other_node graph =
  graph |> assoc node |> exists (fun n -> n = other_node)

(* count number of edge *)
let num_edge node graph = assoc node graph |> length

(* remove a node from a graph *)
let remove node graph =
  graph |> remove_assoc node
  |> map (fun (i, e) -> i, filter (fun n -> n <> node) e)

(* Unfold a lattice to int list list *)
let unfold_lattice graph =
  (* first node of array *)
  let first_node = graph |> find (fun (_, e) -> length e = 2) |> fst in

  (* find inner node that is (1) connected to all nodes of the given node and
   * (2) not the given node. *)
  let find_inner node graph =
    match List.assoc node graph with
    | [left; right] ->
      List.assoc left graph
      |> List.filter (fun pin -> is_connected pin right graph)
      |> List.find (fun n -> node <> n)

    | _ -> assert false
  in

  let rec consume_line node graph =
    let edge = List.assoc node graph
    and graph = remove node graph in
    match edge with
    | [] -> [ node ]
    | [ next ] -> node :: (consume_line next graph)
    | _ -> assert false
  in

  let rec unfold_row node pin graph =
    match List.assoc node graph with
    | [_] -> (* Terminal node *)
      let graph = remove node graph in
      [node], graph

    | [left; right] ->
      let next_node = if left = pin then right else left in
      let next_pin = find_inner node graph in
      let graph = remove node graph in
      let ns, graph = unfold_row next_node next_pin graph in
      node::ns, graph

    | _ -> assert false
  in

  let rec unfold_column node pin graph =
    match List.assoc node graph with
    | [_] -> [consume_line node graph] (* a single line *)
    | [left; right] ->
      let next_node, e = if left = pin || pin = 0 then right, left else left, right in
      let next_pin = find_inner node graph in
      let graph = remove node graph in
      let row, graph = unfold_row e next_pin graph in
      (node::row) :: unfold_column next_node next_pin graph

    | _ -> assert false

  in

  unfold_column first_node 0 graph
