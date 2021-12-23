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

let distance { room; room_size } = function
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

let possible_moves g b = function
  | Room b -> (* moves that an amphipod exits a room *)
    let rec aux i d =
      if 0 <= i && i < 11 && Option.is_none g.hallway.(i) then
        i :: aux (i+d) d
      else
        []
    in
    let i = room_door b in
    (* hallways amphipod can access and is not immediately outside any room *)
    aux (i-1) ~-1 @ aux (i+1) 1
    |> List.filter (Fun.negate is_room_door)
    |> List.map (fun i -> Out (b, i))

  | Hallway i -> (* moves that an amphipod enters a room *)
    (* rule 1: dest room has same broods. *)
    let rule1 = List.for_all ((=) b) g.room.(roomid b) in
    (* rule 2: amphipod can access dest room. *)
    let rule2 = 
      let d = sign @@ room_door b - i in
      let rec aux i =
        if i = room_door b then true
        else if Option.is_some g.hallway.(i) then false
        else aux (i+d)
      in
      aux (i+d)
    in
    if rule1 && rule2 then [In (i, b)] else []

(* gather locations of movable amphipods *)
let gather_loc g : (brood * loc) list =
  (Array.to_list g.hallway
   |> List.mapi (fun i b -> Option.map (fun b -> b, Hallway i) b)
   |> List.filter_map Fun.id)
  @
  ([A; B; C; D]
   |> List.map2 (fun r b -> b, r) (Array.to_list g.room)
   |> List.filter (fun (_, r) -> r <> [])
   |> List.map (fun (b, r) -> List.hd r, Room b))

let cost g b m =
  step_cost b * distance g m

let do_move g = function
  | In (i, b) ->
    let e = Option.get g.hallway.(i) in
    g.hallway.(i) <- None;
    g.room.(roomid b) <- e :: g.room.(roomid b)
  | Out (b, i) ->
    let e = List.hd g.room.(roomid b) in
    g.room.(roomid b) <- List.tl g.room.(roomid b);
    g.hallway.(i) <- Some e

let is_finished { room; room_size } =
  let open List in
  Array.to_list room
  |> map2 (fun b r -> b, r) [A; B; C; D]
  |> for_all (fun (b, r) -> length r = room_size && for_all ((=) b) r)

module M = Map.Make(Int)

let find_min_cost g =
  (* Basically multiset *)
  let dp = ref M.empty in
  let add c m g =
    match M.find_opt c !dp with
    | None -> dp := M.add c (ref [m, g]) !dp
    | Some xs -> xs := (m, g) :: !xs
  in
  let rec next () =
    let c, xs = M.min_binding !dp in
    match !xs with
    | [] -> dp := M.remove c !dp; next ()
    | x::l -> xs := l; c, x
  in

  (* hashtable to track visited graphs *)
  let gc = Hashtbl.create 100 in

  (* Dijkstra *)
  let rec aux (c, (moves, g)) =
    if is_finished g then
      c, moves
    else if Hashtbl.mem gc g then
      (* because if we visited this graph, then that visit was the visit
       * that COST LESS than this visit, we skip this. *)
      aux @@ next ()
    else begin
      Hashtbl.add gc g true; (* we visited this *)
      gather_loc g
      |> List.map (fun (b, loc) -> b, possible_moves g b loc)
      |> List.concat_map (fun (b, moves) ->
          List.map (fun m -> cost g b m, m) moves)
      |> List.iter (fun (cost, move) ->
          let g = copy_graph g in
          do_move g move;
          let cost = cost + c in
          add cost (move::moves) g);
      aux @@ next ()
    end
  in
  let c, moves = aux (0, ([], g)) in
  c

(* hand-parsed data *)
let g = {
    hallway = Array.make 11 None;
    room = [| [ A; D ]; [ C; D ]; [ B; B ]; [ A; C ]; |];
    room_size = 2;
  }

let g2 = {
    hallway = Array.make 11 None;
    room = [| [ A; D; D; D ]; [ C; C; B; D ]; [ B; B; A; B ]; [ A; A; C; C ]; |];
    room_size = 4;
  }

let () =
  let _data = () (* need to parse input *) in
  begin
    (* PART 1 *)
    find_min_cost g
    |> Printf.printf "%d\n";

    (* PART 2 *)
    find_min_cost g2
    |> Printf.printf "%d\n";
  end
