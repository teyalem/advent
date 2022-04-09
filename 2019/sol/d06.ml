open Ut

module StringSet = Set.Make(String)

module OrbitGraph = struct
  type t = (string, StringSet.t) Hashtbl.t

  let print graph =
    Hashtbl.iter (fun a b ->
        Printf.printf "%s:" a;
        StringSet.iter (Printf.printf " %s") b;
        print_newline ())
      graph

  let print_graphviz graph =
    Printf.printf "digraph G {\n";
    Hashtbl.iter (fun a b ->
        StringSet.iter (fun b -> Printf.printf "\"%s\" -> \"%s\"\n" a b) b)
    graph;
    Printf.printf "}"

  let of_list l =
    let objs = "COM" :: List.map snd l in
    objs |> List.map (fun obj ->
        obj, List.find_all (fun (b, _) -> b = obj) l
             |> List.map snd
             |> StringSet.of_list)
    |> List.to_seq
    |> Hashtbl.of_seq

  let parse_line str =
    Scanf.sscanf str "%s@)%s" (fun a b -> a, b)

  let parse sl =
    List.map parse_line sl
    |> of_list

  let rec count_elements obj graph =
    let orbiter = Hashtbl.find graph obj in
    1 + (StringSet.fold (fun obj c -> c + count_elements obj graph) orbiter 0)

  let rec count_orbits obj graph =
    let orbiter = Hashtbl.find graph obj in
    if StringSet.is_empty orbiter
    then 0
    else
      count_elements obj graph - 1
      + (StringSet.fold (fun obj c -> c + count_orbits obj graph) orbiter 0) (* indirect orbit *)

  let to_undirect graph =
    let undirect = Hashtbl.copy graph in
    let union_orbitee obj orbiter =
      let orbitee =
        Hashtbl.to_seq graph
        |> Seq.filter_map (fun (a, b) -> if StringSet.mem obj b then Some a else None)
        |> StringSet.of_seq
      in
      Option.some @@ StringSet.union orbiter orbitee
    in
    Hashtbl.filter_map_inplace union_orbitee undirect;
    undirect

  let redirect graph from_obj =
    let visited = Hashtbl.create @@ Hashtbl.length graph in
    let rec loop tmp = (* BFS *)
      Hashtbl.add visited tmp true;
      let orbiter = Hashtbl.find graph tmp in
      let seq =
        StringSet.to_seq orbiter
        |> Seq.filter (fun o -> not (Hashtbl.mem visited o))
      in
      let new_set = StringSet.of_seq seq in
      Seq.iter (fun o -> loop o) seq;
      Hashtbl.replace graph tmp new_set;
    in
    loop from_obj

  let find_path start_obj end_obj graph =
    let visited = Hashtbl.create @@ Hashtbl.length graph in
    let rec loop tmp = (* BFS *)
      if tmp = end_obj
      then Some 0
      else begin
        Hashtbl.add visited tmp true;
        let orbiter = Hashtbl.find graph tmp in
        StringSet.to_seq orbiter
        |> Seq.filter (fun o -> not (Hashtbl.mem visited o))
        |> Seq.filter_map loop
        |> Seq.map (fun n -> n+1)
        |> Seq.fold_left min max_int
        |> fun n -> if n = max_int then None else Some n
      end
    in
    loop start_obj

end

let () =
  let data = IO.read_lines () |> OrbitGraph.parse in
  begin
    (* PART 1 *)
    OrbitGraph.count_orbits "COM" data |> print_int;

    print_newline ();

    (* PART 2 *)
    let graph = OrbitGraph.to_undirect data in
    let path_len = OrbitGraph.find_path "YOU" "SAN" graph |> Option.get in
    (* YOU -> OBJ, OBJ -> SAN *)
    path_len - 2 |> print_int
    
  end
