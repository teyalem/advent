open Ut

let return = Option.some
let (let*) = Option.bind

module IP = struct
  type t = int * int

  let compare (x, y) (a, b) = (* reading order *)
    match Int.compare y b with
    | 0 -> Int.compare x a
    | v -> v

  let equal a b = compare a b = 0

  let distance (x, y) (v, w) =
    abs (x - v) + abs (y - w)

  let adjacent a b =
    distance a b = 1
end

module S = Set.Make(IP)
module M = Map.Make(IP)

type unit_type = Elf | Goblin

let enemy = function
  | Elf -> Goblin
  | Goblin -> Elf

module Unit = struct
  type t = unit_type * int

  let utype = fst
  let hp = snd

  let is_enemy a b =
    utype a = enemy @@ utype b

  let is_elf (t, _) = t = Elf

  let attack elf_atk (t, hp) =
    t, (match t with Elf -> hp - 3 | Goblin -> hp - elf_atk)

  let is_dead (_, hp) = hp <= 0

  let of_char = function
    | 'E' -> Elf, 200
    | 'G' -> Goblin, 200
    | _ -> assert false
end

module Cave = struct
  type t = char array array

  let get cave (x, y) = cave.(y).(x)
  let set cave (x, y) c = cave.(y).(x) <- c

  let dimx cave = Array.length cave.(0)
  let dimy cave = Array.length cave

  let is_blocked cave (x, y as pos) =
    let dx = dimx cave and dy = dimy cave in
    0 <= x && x < dx && 0 <= y && y < dy
    && (match get cave pos with
        | '#' | '@' -> true
        | _ -> false)
end

let read (ss: string list) = 
  let units = ref M.empty in
  let mat =
    List.map (fun s -> String.to_seq s |> Array.of_seq) ss
    |> Array.of_list
    |> Array.mapi (fun y arr ->
        arr |> Array.mapi (fun x c ->
            if c = 'E' || c = 'G' then begin
              units := M.add (x, y) (Unit.of_char c) !units;
              '@' (* placeholder *)
            end
            else c))
  in
  mat, !units

(* return (pos, int) hashtbl that int means distance from src to pos. *)
let fill cave src =
  let q = Queue.create () in
  let visited = Hashtbl.create 100 in
  let add (pos, _ as t) =
    if not @@ Hashtbl.mem visited pos then Queue.add t q
  in
  let rec pop () =
    let* pos, _ as t = Queue.take_opt q in
    if Hashtbl.mem visited pos then pop () else return t
  in

  let rec aux () =
    match pop () with
    | None -> ()
    | Some (pos, w) ->
      Hashtbl.replace visited pos w;
      Neigh.(neighbors von_neumann pos)
      |> List.filter (Fun.negate @@ Cave.is_blocked cave)
      |> List.map (fun p -> p, w + 1)
      |> List.iter add;
      aux ()
  in
  add (src, 0); aux ();
  visited

(* optionally return least value according to compare by f *)
let find_least compare f = function
  | [] -> None
  | x::xs ->
    List.fold_left (fun a b -> if compare (f a) (f b) <= 0 then a else b) x xs
    |> Option.some

(* return list of minima according to compare by f *)
let find_mins compare f = function
  | [] -> []
  | xs ->
    let least = f @@ Option.get @@ find_least compare f xs in
    List.filter (fun x -> f x = least) xs

(* find a step from src to one of dests *)
let find_move cave src dests =
  let* target =
    fill cave src
    |> Hashtbl.to_seq
    |> Seq.filter (fun (pos, _) -> List.exists ((=) pos) dests)
    |> List.of_seq
    |> find_mins Int.compare snd (* nearest *)
    |> List.map fst
    |> find_least IP.compare Fun.id (* choose *)
  in
  let reverse = fill cave target in (* distance *)
  Neigh.(neighbors von_neumann src)
  |> List.filter_map (fun p ->
      Hashtbl.find_opt reverse p |> Option.map (fun w -> p, w))
  |> find_mins Int.compare snd
  |> List.map fst
  |> find_least IP.compare Fun.id

(* empty position. *)
let die (cave, units) pos =
  Cave.set cave pos '.';
  Some pos, (cave, M.remove pos units)

(* move unit from src to dst. *)
let moveto (_, units as t) src dst =
  let u = M.find src units in
  let _, (cave, units) = die t src in
  Cave.set cave dst '@';
  cave, M.add dst u units

(* attack unit at position. *)
let attack elf_atk (cave, units as t) pos =
  let u = M.find pos units |> Unit.attack elf_atk in
  if Unit.is_dead u then die t pos
  else None, (cave, M.add pos u units)

(* return list of enemies of unit at position. *)
let enemies units pos =
  let me = M.find pos units in
  units
  |> M.bindings
  |> List.filter (fun (_, u) -> Unit.is_enemy me u)

(* move phase of unit at pos. *)
let move_phase (cave, units as t) pos =
  enemies units pos
  |> List.map fst
  |> List.concat_map Neigh.(neighbors von_neumann)
  |> List.filter (fun pos -> not @@ Cave.is_blocked cave pos)
  |> find_move cave pos
  |> Option.fold ~none: (pos, t) ~some: (fun dst -> dst, moveto t pos dst)

(* return list of adjacent enemies of unit at pos. *)
let adjacent_enemies units pos = 
  enemies units pos
  |> List.filter (fun (p, _) -> IP.adjacent pos p)

(* attack phase of unit at pos *)
let attack_phase elf_atk (_, units as t) pos =
  adjacent_enemies units pos
  |> find_mins Int.compare (fun (_, u) -> Unit.hp u)
  |> find_least IP.compare fst
  |> Option.fold ~none: (None, t) ~some: (fun (dst, _) -> attack elf_atk t dst)

let step elf_atk (_, units as t) pos =
  let adjs = adjacent_enemies units pos in
  if adjs = [] then
    let pos, t = move_phase t pos in
    attack_phase elf_atk t pos
  else
    attack_phase elf_atk t pos

let is_simul_end (_, units) =
  let _, u = M.min_binding units in
  M.for_all (fun _ v -> Unit.(utype u = utype v)) units

let round elf_atk (_, units as t) =
  let order = M.to_seq units |> Seq.map fst |> List.of_seq in
  let rec aux t = function
    | [] -> true, t
    | pos::ps ->
      if is_simul_end t then false, t
      else
        let dead, t = step elf_atk t pos in
        (match dead with
         | None -> aux t ps
         | Some p -> aux t @@ List.filter (fun q -> not @@ IP.equal p q) ps)
  in
  aux t order

let outcome (i, (_, units)) =
  let hps = M.fold (fun _ u acc -> acc + Unit.hp u) units 0 in
  i * hps

let simul elf_atk t =
  let rec aux i t =
    let full, t = round elf_atk t in
    if i > 100 then failwith "oversimulated" else (* for stopping infinite looping *)
    if is_simul_end t then (i + if full then 1 else 0), t
    else aux (i+1) t
  in
  aux 0 t

let copy (cave, units) =
  Array.(map copy cave), units

let simul_elf_win (_, units as t) =
  let num_elf = M.filter (fun _ u -> Unit.is_elf u) units |> M.cardinal in
  let rec aux elf_atk =
    let t = copy t in
    let r, (_, units as t) = simul elf_atk t in
    if M.for_all (fun _ u -> Unit.utype u = Elf) units
    && M.cardinal units = num_elf
    then outcome (r, t)
    else aux (elf_atk + 1)
  in
  aux 4

let () =
  let data =
    Sys.argv.(1)
    |> open_in
    |> In_channel.input_all
    |> String.split_on_char '\n'
    |> read
  in

  (* PART 1 *)
  copy data |> simul 3 |> outcome |> print_int;
  print_newline ();

  (* PART 2 *)
  simul_elf_win data |> print_int; (* takes a while *)
