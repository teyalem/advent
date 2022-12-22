open Ut

type com =
  | Advance of int
  | Turn of string

let parse_path str =
  Str.(full_split (regexp "[LR]") str)
  |> List.map (function
      | Str.Text n -> Advance (int_of_string @@ String.trim n)
      | Delim c -> Turn c)

let parse str =
  match Str.(split (regexp "\n\n") str) with
  | [map; path] ->
    let map =
      map
      |> String.split_on_char '\n'
      |> List.map (fun l -> l |> String.to_seq |> Array.of_seq)
      |> Array.of_list
    in
    let path = parse_path path in
    map, path
  | _ -> assert false

module V = struct
  type t = int * int

  let map2 f (x, y) (u, v) = f x u, f y v
  let add = map2 (+)
  let mul = map2 ( * )
  let scale n (x, y) = n*x, n*y

  let rot_clock (x, y) = -y, x
  let rot_counter (x, y) = y, -x
end

let get_def m y x =
  try Array.(get (get m y) x)
  with _ -> ' '

let find_first_column ~rev map row =
  Array.to_seq map.(row)
  |> Seq.mapi (fun i n -> i, n)
  |> (fun s -> if rev then s |> List.of_seq |> List.rev |> List.to_seq else s)
  |> Seq.find_map (fun (i, n) ->
      if n <> ' ' then Some (i, n) else None)
  |> Option.get

let find_first_row ~rev map column =
  Array.to_seq map
  |> Seq.mapi (fun i arr -> i, arr)
  |> (fun s -> if rev then s |> List.of_seq |> List.rev |> List.to_seq else s)
  |> Seq.find_map (fun (i, arr) ->
      let n = try arr.(column) with _ -> ' ' in
      if n <> ' ' then Some (i, n) else None)
  |> Option.get

let move map (pos, (dx, dy as delta)) com =
  let rec aux pos delta n =
    if n = 0 then pos
    else
      let x, y = V.add pos delta in
      let npos, c =
        if get_def map y x = ' '
        then
          if dx = 0 then
            let row, c = find_first_row ~rev: (dy < 0) map x in
            (x, row), c
          else
            let col, c = find_first_column ~rev: (dx < 0) map y in
            (col, y), c
        else
          (x, y), map.(y).(x)
      in
      if c = '#' then pos else aux npos delta (n-1)
  in
  match com with
  | Advance n -> aux pos delta n, delta
  | Turn c ->
    if c = "L" then pos, V.rot_counter delta
    else if c = "R" then pos, V.rot_clock delta
    else assert false

(* hardcoded region *)
let regions = [
   (1, 0), 0;
   (2, 0), 1;
   (1, 1), 2;
   (0, 2), 3;
   (1, 2), 4;
   (0, 3), 5;
]

let region (x, y) =
  List.assoc (x/50, y/50) regions

let base_pos r =
  regions
  |> List.map (fun (a, b) -> b, a)
  |> List.assoc r
  |> (fun (x, y) -> x*50, y*50)

let next_pos region (x, y) facing =
  let x, y = x mod 50, y mod 50 in
  match region, facing with
  | 0, 2 ->
    let bx, by = base_pos 3 in
    (bx, by + 49 - y), (1, 0)
  | 0, 3 ->
    let bx, by = base_pos 5 in
    (bx, by + x), (1, 0)

  | 1, 0 ->
    let bx, by = base_pos 4 in
    (bx + 49, by + 49 - y), (-1, 0)
  | 1, 1 ->
    let bx, by = base_pos 2 in
    (bx + 49, by + x), (-1, 0)
  | 1, 3 ->
    let bx, by = base_pos 5 in
    (bx + x, by + 49), (0, -1)

  | 2, 0 ->
    let bx, by = base_pos 1 in
    (bx + y, by + 49), (0, -1)
  | 2, 2 ->
    let bx, by = base_pos 3 in
    (bx + y, by), (0, 1)

  | 3, 2 ->
    let bx, by = base_pos 0 in
    (bx, by + 49 - y), (1, 0)
  | 3, 3 ->
    let bx, by = base_pos 2 in
    (bx, by + x), (1, 0)

  | 4, 0 ->
    let bx, by = base_pos 1 in
    (bx + 49, by + 49 - y), (-1, 0)
  | 4, 1 ->
    let bx, by = base_pos 5 in
    (bx + 49, by + x), (-1, 0)

  | 5, 0 ->
    let bx, by = base_pos 4 in
    (bx + y, by + 49), (0, -1)
  | 5, 1 ->
    let bx, by = base_pos 1 in
    (bx + x, by), (0, 1)
  | 5, 2 ->
    let bx, by = base_pos 0 in
    (bx + y, by), (0, 1)

  | _ -> assert false

let facing = function
  | 1, 0 -> 0
  | 0, 1 -> 1
  | -1, 0 -> 2
  | 0, -1 -> 3
  | _ -> assert false

let move2 map (pos, delta) com =
  let rec aux pos delta n =
    if n = 0 then pos, delta
    else
      let x, y = V.add pos delta in
      let (x, y as npos), ndelta =
        if get_def map y x = ' '
        then next_pos (region pos) (x, y) (facing delta)
        else (x, y), delta
      in
      if get_def map y x = '#'
      then pos, delta
      else aux npos ndelta (n-1)
  in
  match com with
  | Advance n -> aux pos delta n
  | Turn c ->
    if c = "L" then pos, V.rot_counter delta
    else if c = "R" then pos, V.rot_clock delta
    else assert false

let password ((x, y), delta) =
  1000*(y+1) + 4*(x+1) + facing delta

let () =
  let map, path = open_in Sys.argv.(1) |> In_channel.input_all |> parse in
  let x, _ = find_first_column ~rev: false map 0 in
  (* PART 1 *)
  List.fold_left (move map) ((x, 0), (1, 0)) path
  |> password
  |> print_int;
  print_newline ();

  (* PART 2 *)
  (* NOTE: Part 2 only works in my input. *)
  List.fold_left (move2 map) ((x, 0), (1, 0)) path
  |> password
  |> print_int;
  print_newline ()
