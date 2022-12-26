open Ut

let _NONE = -1
let _ORE = 0
let _CLAY = 1
let _OBSIDIAN = 2
let _GEODE = 3

let restype = function
  | "ore" -> _ORE
  | "clay" -> _CLAY
  | "obsidian" -> _OBSIDIAN
  | "geode" -> _GEODE
  | _ -> assert false

let empty = Array.make 4 0

let parse str =
  let n, rest = Scanf.sscanf str "Blueprint %d: %s@!" (fun n rest -> n, rest) in
  let bots =
    String.split_on_char '.' rest
    |> List.map String.trim
    |> List.filter (fun str -> str <> "")
    |> List.map (fun str ->
        Scanf.sscanf str "Each %s robot costs %s@!" (fun t rest ->
            restype t,
            Str.(split (regexp "and") rest)
            |> List.map String.trim
            |> List.map (fun l -> Scanf.sscanf l "%d %s" (fun n t -> t, n))
            |> (fun l ->
                let arr = Array.make 4 0 in
                List.iter (fun (t, n) -> arr.(restype t) <- n) l;
                arr
             )))
    |> (fun l -> Array.init 4 (fun bt -> List.assoc bt l))
  in
  n, bots

type state = {
  min : int;
  bots : int array;
  res : int array;
}

let zero_state = {
  min = 0;
  bots = [| 1; 0; 0; 0 |];
  res = Array.make 4 0;
}

let makeable_bots state (bp, max_use) =
  bp
  |> Array.to_list
  |> List.mapi (fun i n -> i, n)
  |> List.filter_map (fun (bt, res) ->
      if Array.for_all2 (>=) state.res res && state.bots.(bt) < max_use.(bt)
      then Some bt
      else None)
  |> (fun l ->
      if List.mem _GEODE l then [_GEODE]
      else if List.mem _OBSIDIAN l then [_NONE; _OBSIDIAN]
      else _NONE :: l)

let add = Array.map2 (+)
let sub = Array.map2 (-)

let next_states state (bp, max_use) =
  makeable_bots state (bp, max_use)
  |> List.map (fun bt ->
      let bots = Array.copy state.bots in
      let spending = if bt = _NONE then empty else bp.(bt) in
      if bt <> _NONE then bots.(bt) <- bots.(bt) + 1;
      let res = sub (add state.res state.bots) spending in
      { min = state.min + 1;
        bots;
        res; })

let dfs nmin bp =
  let max_use = Array.fold_left (Array.map2 max) empty bp in
  max_use.(_GEODE) <- max_int;
  let max_geode = ref 0 in
  let rec aux state =
    let geode = state.res.(_GEODE) in
    if state.min = nmin then
      max_geode := max !max_geode geode
    else if
      let m = nmin - state.min + 1 in
      let geode_bot = state.bots.(_GEODE) in
      geode + geode_bot*m + (m*(m+1) / 2) < !max_geode
    then ()
    else next_states state (bp, max_use) |> List.iter aux
  in
  aux zero_state;
  !max_geode

let () =
  let data = open_in Sys.argv.(1) |> IO.input_lines |> List.map parse in
  (* PART 1 *)
  List.map (fun (i, bp) -> i, dfs 24 bp) data
  |> List.map (fun (i, s) -> Printf.printf "%d %d\n" i s; i * s)
  |> List.fold_left (+) 0
  |> print_int;
  print_newline ();

  (* PART 2 *)
  (* takes 24 minutes *)
  (* doesn't return right numbers for test input *)
  List.to_seq data
  |> Seq.take 3
  |> Seq.map (fun (_, bp) -> dfs 32 bp)
  |> Seq.fold_left Int.mul 1
  |> print_int
