open Ut

let read_map ss =
  let tbl = Hashtbl.create 500 in
  let h = List.length ss / 2 in
  ss |> List.iteri (fun y ->
      String.iteri (fun x c ->
          if c = '#' then Hashtbl.add tbl (x - h, y - h) true));
  tbl

let turn_right (x, y) = ~-y, x
let turn_left (x, y) = y, ~-x

let (+$) (a, b) (c, d) = a+c, b+d

let get ~default map pos =
  Hashtbl.find_opt map pos
  |> Option.value ~default

let set map pos x =
  Hashtbl.replace map pos x

let is_infected = get ~default: false

let burst1 (map, pos, dir, acc) =
  let dir = (if is_infected map pos then turn_right else turn_left) dir in
  let n = not @@ is_infected map pos in
  set map pos n;
  map, pos +$ dir, dir, acc + (if n then 1 else 0)

type t = Clean | Weakened | Infected | Flagged

let progress = function
  | Clean -> Weakened
  | Weakened -> Infected
  | Infected -> Flagged
  | Flagged -> Clean

let burst2 (map, pos, dir, acc) =
  let dir =
    (match get ~default: Clean map pos with
    | Clean -> turn_left
    | Weakened -> Fun.id
    | Infected -> turn_right
    | Flagged -> (fun (x, y) -> ~-x, ~-y))
    dir
  in
  let n = progress @@ get ~default: Clean map pos in
  set map pos n;
  map, pos +$ dir, dir, acc + (if n = Infected then 1 else 0)

let () =
  let data = IO.read_lines () |> read_map in
  let solve map f n =
    Seq.iterate f (map, (0, 0), (0, -1), 0)
    |> Seq.drop n
    |> Seq.take 1
    |> Seq.iter (fun (_, _, _, acc) -> print_int acc);
  in
  (* PART 1 *) solve (Hashtbl.copy data) burst1 10_000;
  print_newline ();

  let map =
    Hashtbl.to_seq data
    |> Seq.map (function pos, false -> pos, Clean | pos, true -> pos, Infected)
    |> Hashtbl.of_seq
  in
  (* PART 2 *) solve map burst2 10_000_000;
