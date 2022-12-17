open Ut

let to_pos rock =
  let l = List.length rock in
  rock
  |> List.mapi (fun y s ->
      String.to_seq s
      |> Seq.mapi (fun x c ->
          if c = '#' then Some (x, l-1-y)
          else None)
      |> Seq.filter_map Fun.id
      |> List.of_seq)
  |> List.concat

let rocks = [ (* zero is bottom left *)
  ["####";];

  [".#.";
   "###";
   ".#.";];

  ["..#";
   "..#";
   "###";];

  ["#";
   "#";
   "#";
   "#";];

  ["##";
   "##";]
]
  |> List.map to_pos
  |> List.mapi (fun i r -> i, r)
  |> List.to_seq
  |> Seq.cycle

module Map = struct
  type t = {
    (* coordination: x is 0..7, y up is plus *)
    map : ((int * int), bool) Hashtbl.t;
    mutable highest_rock : int;
  }

  let make () = {
    map = Hashtbl.create 100;
    highest_rock = 0;
  }

  let copy map =
    { map = Hashtbl.copy map.map;
      highest_rock = map.highest_rock;
    }

  let highest map = map.highest_rock

  let spawn_place { highest_rock; _ } =
    2, highest_rock + 3

  let landed map (rx, ry) rock =
    List.exists (fun (x, y) ->
        ry + y = 0 || Hashtbl.mem map.map (rx+x, ry+y-1))
      rock

  let is_blocked map (rx, ry) rock =
    List.exists (fun (x, y) ->
        let x, y = rx + x, ry + y in
        not (0 <= x && x < 7)
        || y < 0
        || Hashtbl.mem map.map (x, y))
      rock

  let place map (rx, ry) rock =
    let my = List.(fold_left max min_int @@ map snd rock) in
    List.iter (fun (x, y) ->
        Hashtbl.add map.map (rx+x, ry+y) true)
      rock;
    map.highest_rock <- max map.highest_rock (ry + my + 1)

  let top_array map =
    Array.init 3 (fun y ->
        Array.init 7 (fun x ->
            Hashtbl.mem map.map (x, map.highest_rock - 3 + y)))

  let print_slice { map; highest_rock } =
    for y = highest_rock - 1 downto highest_rock - 1 do
      for x = 0 to 6 do
        if Hashtbl.mem map (x, y)
        then print_char '#'
        else print_char '.'
      done;
      print_newline ()
    done

end

let fall (map, jet_pattern) (_rocki, rock) =
  let step (x, y as _pos) jet_pattern =
    let (_i, c), seq = Seq.uncons jet_pattern |> Option.get in
    let nx =
      if c = '<' then x-1
      else if c = '>' then x+1
      else assert false
    in
    let x =
      if Map.is_blocked map (nx, y) rock
      then x
      else nx
    in
    let ny = y - 1 in

    (*
    if rocki = 0 && i = 0 then begin
      let h = Map.highest map in
      print_newline ();
      for y = h + 1 downto h - 3 do
        for x = 0 to 6 do
          print_char (
            if Hashtbl.mem map.map (x, y) then '#'
            else if List.exists (fun (dx, dy) -> (x+dx, y+dy) = pos) rock then '@'
            else '.')
        done;
        print_newline ()
      done
    end;
       *)

    if Map.is_blocked map (x, ny) rock
    then true, (x, y), seq
    else false, (x, ny), seq
  in
  let rec aux pos jet_pattern =
    let isend, pos, jet_pattern = step pos jet_pattern in
    if isend then pos, jet_pattern
    else aux pos jet_pattern
  in
  let pos, jet_pattern = aux (Map.spawn_place map) jet_pattern in
  let map = Map.copy map in
  Map.place map pos rock;
  map, jet_pattern

let simul jet_pattern n =
  let map = Map.make () in
  Seq.take n rocks
  |> Seq.fold_left fall (map, jet_pattern)
  |> fst
  |> Map.highest

let simul2 jet_pattern n =
  let map = Map.make () in
  let rs = Seq.take n rocks in
  Seq.unfold (fun (map, jet_pattern, rocks) ->
      Seq.uncons rocks
      |> Option.map (fun (rock, rocks) ->
          let map, jet_pattern = fall (map, jet_pattern) rock in
          Map.highest map, (map, jet_pattern, rocks)))
    (map, jet_pattern, rs)

(* from https://github.com/HoshigaIkaro/aoc-2022/blob/main/src/days/day_17.rs *)
let find_cycle seq =
  Seq.init 500 Fun.id
  |> Seq.find_map (fun offset ->
      let deltas = Seq.drop offset seq in
      Seq.init 2501 Fun.id (* 2..=2500 *)
      |> Seq.drop 2
      |> Seq.find (fun size ->
          let window = Seq.(take size deltas |> cycle) in
          Seq.for_all2 (=) window deltas)
      |> Option.map (fun size -> offset, size))

let () =
  let data =
    open_in Sys.argv.(1)
    |> input_line
    |> String.to_seq
    |> Seq.mapi (fun i c -> i, c)
    |> Seq.memoize
  in
  let data = Seq.cycle data in

  (* PART 1 *)
  simul data 2022 |> print_int;
  print_newline ();

  (* PART 2 *)
  (* from https://github.com/HoshigaIkaro/aoc-2022/blob/main/src/days/day_17.rs *)
  let n = 1_000_000_000_000 in
  let deltas =
    simul2 data 5000
    |> Seq.scan (fun (_delta, p) n -> n - p, n) (0, 0)
    |> Seq.map fst
    |> Seq.drop 1
    |> Seq.memoize
  in
  let sum = Seq.fold_left (+) 0 in
  let offset, size = deltas |> find_cycle |> Option.get in

  let offset_delta = deltas |> Seq.take offset |> sum in
  let cycle_deltas = deltas |> Seq.drop offset |> Seq.take size in
  let cycle_delta = cycle_deltas |> sum in

  let n = n - offset in
  let cycle_count = n / size in
  let remaining = cycle_deltas |> Seq.take (n mod size) |> sum in
  let height = offset_delta + cycle_count*cycle_delta + remaining in
  print_int height
