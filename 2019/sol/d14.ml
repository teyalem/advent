open Ut

let int_ceiling n base =
  if n mod base = 0
  then n
  else n - n mod base + base

module Reaction = struct
  type chem = int * string

  let amount (i, _) = i
  let name (_, chem) = chem

  type t = chem * chem list

  let parse str =
    let parse_chem str =
      Scanf.sscanf str " %d %s" (fun n chem -> n, chem)
    in

    let parse_source_list str =
      Delim.split "," str
      |> List.map String.trim
      |> List.map parse_chem
    in

    Scanf.sscanf str "%s@=> %d %s" (fun source n chem ->
        (n, chem), parse_source_list source)

  let print reactions =
    List.iter (fun (chem, source) ->
        List.iter (fun (n, c) -> Printf.printf " %d %s," n c) source;
        Printf.printf "=> %d %s\n" (amount chem) (name chem);)
      reactions

  let find chem reactions =
    try List.find (fun (c, _) -> name c = chem) reactions
    with _ -> failwith chem

  let find_req chem reactions =
    find chem reactions |> snd

  let find_base chem reactions =
    find chem reactions |> fst |> fst

  let find_outputs chem reactions =
    List.filter (fun (_, sources) ->
        List.exists (fun c -> name c = chem) sources) reactions
    |> List.map fst

  let level_graph reactions =
    let rec loop tmp (last: string list) =
      if List.hd last = "FUEL"
      then last::tmp
      else
        let next = List.concat_map (fun c -> find_outputs c reactions) last
                   |> List.map (fun c -> name c)
                   |> List.sort_uniq compare
        in
        loop (last::tmp) next
    in
    loop [] ["ORE"]
    |> List.mapi (fun i l -> i, l)
    |> List.concat_map (fun (i, l) -> List.map (fun n -> n, i) l)

  let level chem levels =
    List.assoc chem levels

  let min_level chems levels =
    List.map (fun c -> level (name c) levels) chems |> List.fold_left min max_int

  let group_same_chem chems =
    List.sort (fun a b -> compare (name a) (name b)) chems
    |> List.fold_left (fun p (n, c) ->
        match p with
          [] -> [n, c]
        | (i, chem) :: rest ->
          if c = chem
          then (n+i, chem) :: rest
          else (n, c) :: (i, chem) :: rest)
      []

  let simplify_list chems levels reactions =
    let open List in
    let l = min_level chems levels in
    let ps, ns = partition (fun chem -> level (name chem) levels = l) chems in
    let ps = ps
             |> concat_map (fun chem ->
                 find_req (name chem) reactions
                 |> map (fun (n, c) ->
                     let base = find_base (name chem) reactions in
                         (amount chem)/base*n, c))
    in
    let next = group_same_chem (ps @ ns) in
    let l = min_level next levels in
    let pps, nns = partition (fun chem -> level (name chem) levels = l) next in
    let pps = pps
              |> map (fun (n, chem) ->
                  if chem = "ORE"
                  then n, chem
                  else
                    let base = find_base chem reactions in
                    int_ceiling n base, chem)
    in
    group_same_chem (pps @ nns)


  let calculate_ore chems reactions =
    let levels = level_graph reactions in
    let rec loop = function
        [] -> assert false
      | [i, "ORE"] -> i
      | chems -> loop @@ simplify_list chems levels reactions
    in
    loop chems

end

let () =
  let data = IO.read_lines () |> List.map Reaction.parse in
  begin
    (* PART 1 *)
    let one_fuel = Reaction.calculate_ore [1, "FUEL"] data in
    print_int one_fuel;

    print_newline ();

    (* PART 2 *)
    (* thanks to u/Diderikdm *)
    let ore_amount = 1_000_000_000_000 in
    let lower_bound = ore_amount / one_fuel in
    let f = Reaction.calculate_ore [lower_bound, "FUEL"] data in
    let offset = float ore_amount /. float f in
    let fuels = int_of_float (float lower_bound *. offset) in
    print_int fuels;

  end
