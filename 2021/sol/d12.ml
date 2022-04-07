open Ut

module M = Map.Make(String)

let is_uppercase c =
  let c = Char.code c in
  Char.(code 'A' <= c && c <= code 'Z')

let is_lowercase c =
  let c = Char.code c in
  Char.(code 'a' <= c && c <= code 'z')

let to_graph xs =
  let add a b g =
    match M.find_opt a g with
    | None -> M.add a [b] g
    | Some l -> M.add a (b::l) g
  in
  List.fold_left (fun g (a, b) -> add a b g |> add b a)
    M.empty
    xs

let parse str =
  Scanf.sscanf str "%s@-%s" (fun a b -> a, b)

let collect_paths_part1 g =
  let rec aux rpath =
    let h = List.hd rpath in
    if h = "end" then [rpath]
    else
      M.find h g
      |> List.filter (fun nb ->
          is_uppercase nb.[0] || not @@ List.mem nb rpath)
      |> List.concat_map (fun nb -> aux (nb :: rpath))
  in
  aux ["start"]

let collect_paths_part2 g =
  let rec aux rpath twice =
    let h = List.hd rpath in
    if h = "end" then [rpath]
    else
      M.find h g
      |> List.filter (fun nb ->
          nb <> "start"
          && (is_uppercase nb.[0]
              || not @@ List.mem nb rpath
              || not twice))
      |> List.concat_map (fun nb ->
          aux (nb :: rpath)
            (twice || is_lowercase nb.[0] && List.mem nb rpath))
  in
  aux ["start"] false

let () =
  let data = IO.input_lines stdin |> List.map parse |> to_graph in
  begin
    (* PART 1 *)
    collect_paths_part1 data
    |> List.length
    |> Printf.printf "%d\n";

    (* PART 2 *)
    collect_paths_part2 data
    |> List.length
    |> Printf.printf "%d\n";
  end
