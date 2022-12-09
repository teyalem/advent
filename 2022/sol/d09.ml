open Ut

let directions = [
  'U', (0, -1);
  'D', (0, 1);
  'L', (-1, 0);
  'R', (1, 0);
]

let parse str =
  Scanf.sscanf str "%c %d" (fun d s -> List.assoc d directions, s)

let move (x, y) (dx, dy) =
  x+dx, y+dy

let diff (x1, y1) (x2, y2) =
  x1-x2, y1-y2

let compare_pair (a, b) (x, y) =
  match Int.compare a x with
  | 0 -> Int.compare b y
  | v -> v

let sign n =
  if n > 0 then 1
  else if n = 0 then 0
  else -1

let reposition h t =
  let dx, dy = diff h t in
  if abs dx > 1 || abs dy > 1
  then move t (sign dx, sign dy)
  else t

let step1 (h, t) d =
  let nh = move h d in
  let nt = reposition nh t in
  nh, nt

let step2 xs d =
  let rec aux p = function
    | [] -> [p]
    | x::xs ->
      p :: aux (reposition p x) xs
  in
  match xs with
  | [] -> assert false
  | h::tails -> aux (move h d) tails

let solve f last z data =
  let rec aux (log, p) (d, s) =
    if s = 0 then log, p
    else
      let n = f p d in
      aux ((last n)::log, n) (d, s-1)
  in
  List.fold_left aux ([], z) data
  |> fst
  |> List.sort_uniq compare_pair
  |> List.length

let rec last = function
  | [] -> assert false
  | [x] -> x
  | _::xs -> last xs

let () =
  let data = open_in Sys.argv.(1) |> IO.input_lines |> List.map parse in
  let z = 0, 0 in
  solve step1 snd (z, z) data |> print_int;
  print_newline ();

  let zs = List.init 10 @@ Fun.const z in
  solve step2 last zs data |> print_int
