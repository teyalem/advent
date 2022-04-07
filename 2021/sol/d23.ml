open Ut

let sign n =
  if n < 0 then -1
  else if n > 0 then 1
  else 0

type brood = A | B | C | D

type graph = {
  hallway : brood option array;
  room : brood list array;
  room_size : int;
}

type loc = Room of brood | Hallway of int

type move =
  | In of int * brood
  | Out of brood * int

let brood_of_char = function
  | 'A' -> A
  | 'B' -> B
  | 'C' -> C
  | 'D' -> D
  | _ -> assert false

let step_cost = function
  | A -> 1
  | B -> 10
  | C -> 100
  | D -> 1000

let roomid = function
  | A -> 0
  | B -> 1
  | C -> 2
  | D -> 3

let room_door = function
  | A -> 2
  | B -> 4
  | C -> 6
  | D -> 8

let is_room_door = function
  | 2 | 4 | 6 | 8 -> true
  | _ -> false

let distance { room; room_size; _ } = function
  | In (i, b) ->
    abs (i - room_door b)
    + (room_size - List.length room.(roomid b))
  | Out (b, i) ->
    abs (i - room_door b)
    + (room_size + 1 - List.length room.(roomid b))

let copy_graph g = {
  hallway = Array.copy g.hallway;
  room = Array.copy g.room;
  room_size = g.room_size;
}

let blocking_amphi { hallway; _ } b i =
  let d = sign @@ room_door b - i in
  let rec aux i =
    if i = room_door b then None
    else if Option.is_some hallway.(i) then Some i
    else aux (i+d)
  in
  aux (i+d)

let is_reachable g b i =
  Option.is_none @@ blocking_amphi g b i

(* generate possible moves of b amphipod at location *)
let possible_moves g b = function
  | Room b -> (* moves that an amphipod exits a room *)
    let rec aux i d =
      if 0 <= i && i < 11 && Option.is_none g.hallway.(i) then
        i :: aux (i+d) d
      else
        []
    in
    if List.for_all ((=) b) g.room.(roomid b) then
      (* don't need to move out if already in correct room with same brood *)
      []
    else
      (* hallways accessable and not immediately outside any room *)
      let i = room_door b in
      aux (i-1) ~-1 @ aux (i+1) 1
      |> List.filter (Fun.negate is_room_door)
      |> List.map (fun i -> Out (b, i))

  | Hallway i -> (* moves that an amphipod enters a room *)
    (* rule 1: dest room has no amphis or same broods. *)
    let rule1 = List.for_all ((=) b) g.room.(roomid b) in
    (* rule 2: amphipod can access dest room. *)
    let rule2 = is_reachable g b i in
    if rule1 && rule2 then [In (i, b)] else []

(* gather locations of movable amphipods *)
let gather_loc g : (brood * loc) list =
  (* amphipods at hallway *)
  (Array.to_list g.hallway
   |> List.mapi (fun i b -> Option.map (fun b -> b, Hallway i) b)
   |> List.filter_map Fun.id)
  @ (* amphipods at rooms *)
  ([A; B; C; D]
   |> List.map2 (fun r b -> b, r) (Array.to_list g.room)
   |> List.filter (fun (_, r) -> r <> [])
   |> List.map (fun (b, r) -> List.hd r, Room b))

(* cost of the move m of amphipod b *)
let cost g b m =
  step_cost b * distance g m

let do_move { hallway; room; _ } = function
  | In (i, b) ->
    let e = Option.get hallway.(i) in
    hallway.(i) <- None;
    room.(roomid b) <- e :: room.(roomid b)
  | Out (b, i) ->
    let e = List.hd room.(roomid b) in
    room.(roomid b) <- List.tl room.(roomid b);
    hallway.(i) <- Some e

let is_stuck g loc =
  loc
  |> List.exists (function
      | b, Hallway i -> begin
          match blocking_amphi g b i with
          | None -> false
          | Some j ->
            let b = Option.get g.hallway.(i) in
            not @@ is_reachable g b j
        end
      | _, _ -> false)

let is_finished { room; room_size; _ } =
  let open List in
  Array.to_list room
  |> map2 (fun b r -> b, r) [A; B; C; D]
  |> for_all (fun (b, r) -> length r = room_size && for_all ((=) b) r)

let find_min_cost g =
  (* Basically (not-so-slow) prioriry queue (bucket queue?) *)
  let module M = Map.Make(Int) in
  let dp = ref M.empty in
  let add c g =
    match M.find_opt c !dp with
    | None -> dp := M.add c (ref [g]) !dp
    | Some xs -> xs := g :: !xs
  in
  let rec next () =
    let c, l = M.min_binding !dp in
    match !l with
    | [] -> dp := M.remove c !dp; next ()
    | x::tl -> l := tl; c, x
  in

  (* hashtable to track visited graphs *)
  let gc = Hashtbl.create 100 in

  (* Dijkstra *)
  let rec aux (c, g) =
    if is_finished g then
      c
    else if Hashtbl.mem gc g then
      (* because if we visited this graph, then that visit was the visit
       * that COST LESS than this visit, we skip this. *)
      aux @@ next ()
    else begin
      Hashtbl.add gc g c; (* we visited this *)
      gather_loc g
      |> List.map (fun (b, loc) -> b, possible_moves g b loc)
      |> List.concat_map (fun (b, moves) ->
          List.map (fun m -> cost g b m, m) moves)
      |> List.iter (fun (cost, move) ->
          let g = copy_graph g in
          do_move g move;
          add (c + cost) g);
      aux @@ next ()
    end
  in
  aux (0, g)

let parse ss =
  let open List in
  let bs =
    drop 2 ss
    |> map String.to_seq
    |> map (Seq.filter (fun c -> c <> '#' && c <> ' '))
    |> map of_seq
    |> filter (fun l -> l <> [])
  in
  let room =
    Array.init 4 (fun i ->
        map (fun cs -> brood_of_char @@ nth cs i) bs)
  in
  { hallway = Array.make 11 None;
    room;
    room_size = List.length room.(0); }

let hidden = [
  "#D#C#B#A#";
  "#D#B#A#C#";
]

let unfold_paper ss =
  take 3 ss @ hidden @ drop 3 ss

let () =
  let data = IO.read_lines () in
  let solve data =
    parse data |> find_min_cost |> Printf.printf "%d\n"
  in
  begin
    (* PART 1 *) solve data;
    (* PART 2 *) solve @@ unfold_paper data;
  end
