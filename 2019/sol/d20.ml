open Ut

type loc = Outer | Inner

let flip = function
  | Inner -> Outer
  | Outer -> Inner

let compare_loc a b =
  match a, b with
  | Outer, Inner -> 1
  | Inner, Outer -> -1
  | _ -> 0

module T = struct
  type t = Empty | Floor | Wall | Portal of string

  let default = Empty

  let to_char = function
    | Empty -> ' '
    | Floor -> '.'
    | Wall -> '#'
    | Portal p ->
      if String.length p = 1 then p.[0] else '@'

  let of_char = function
    | ' ' -> Empty
    | '.' -> Floor
    | '#' -> Wall
    | c -> Portal (String.make 1 c)
             
  let is_portal = function
    | Portal _ -> true
    | _ -> false

  let get_portal_name = function
    | Portal p -> p
    | _ -> ""

  let join_portal a b =
    match a, b with
    | Portal a, Portal b -> Portal (a ^ b)
    | _ -> assert false
end

module Map = struct
  include Block.Make(T)

  let get_opt map x y =
    try get map x y
    with _ -> T.Empty

  let locof map x y =
    let dx = dimx map and dy = dimy map in
    let b = 3 in
    if b <= x && x < dx-b && b <= y && y < dy-b
    then Inner
    else Outer

  let join_portals map =
    let vert_portal x y =
      T.join_portal (get map x y) (get map x (y+1))
    and horz_portal x y =
      T.join_portal (get map x y) (get map (x+1) y)
    in
    let place (x, y) (cx, cy) p =
      set map x y p;
      set map cx cy T.Empty
    in
    map |> iteri (fun x y c ->
        match c with
        | Portal c when String.length c = 1 ->
          if get map (x+1) y |> T.is_portal then
            let p = horz_portal x y in
            if get_opt map (x-1) y = T.Floor
            then place (x, y) (x+1, y) p
            else place (x+1, y) (x, y) p
          else if get map x (y+1) |> T.is_portal then
            let p = vert_portal x y in
            if get_opt map x (y-1) = T.Floor
            then place (x, y) (x, y+1) p
            else place (x, y+1) (x, y) p
          else assert false
        | _ -> ());
    map

  let collect_portals map =
    let ps = ref [] in
    map |> iteri (fun x y -> function
        | T.Portal p -> ps := (x, y, p) :: !ps
        | _ -> ());
    !ps

end

module G : Pathfind.StateSpace
  with type space = Map.t
   and type state = int * int
   and type data = string * loc * int =
struct
  type space = Map.t
  type state = int * int
  type data = string * loc * int

  let data_id = "", Outer, -1

  let is_end _ (_, (p, _, d)) =
    p <> "" && d <> 0

  let neighbors map (p, (_, _, d)) =
    Neigh.(neighbors von_neumann p)
    |> List.filter (fun (x, y) ->
        0 <= x && x < Map.dimx map && 0 <= y && y < Map.dimy map)
    |> List.filter (fun (x, y) ->
        match Map.get map x y with
        | T.Floor | T.Portal _ -> true
        | _ -> false)
    |> List.map (fun (x, y) ->
        let name = Map.get map x y |> T.get_portal_name in
        (x, y), (name, Map.locof map x y, d + 1))
end

module IP = struct
  type t = int * int
  let compare (a, b) (c, d) =
    match Int.compare a c with
    | 0 -> Int.compare b d
    | v -> v
end

module SL = struct
  type t = string * loc
  let compare (a, b) (c, d) =
    match String.compare a c with
    | 0 -> compare_loc b d
    | v -> v
end

module M = Stdlib.Map.Make(SL)

type graph =  ((string * loc * int) list) M.t

let make_graph map : graph =
  Map.collect_portals map
  |> List.map (fun (x, y, p) ->
      (p, Map.locof map x y),
      Pathfind.bfs_collect (module IP) (module G) ~start: (x, y) map)
  |> List.to_seq
  |> M.of_seq

let print_graph g =
  let f l = match l with Inner -> "inner" | Outer -> "outer" in
  let pf = Printf.printf in
  pf "digraph {\n";
  g |> M.iter (fun (k, l) v ->
      v |> List.iter (fun (p, loc, d) ->
          pf "%s_%s -> %s_%s [label=%d]\n" k (f l) p (f loc) d));
  pf "}"

module Donut = struct
  type space = graph
  type state = string * loc
  type data = string list
  type weight = int

  let data_id = []

  let is_end _ ((p, _), _) =
    p = "ZZ"

  let neighbors g ((p, o), ps) =
    M.find (p, o) g
    |> List.map (fun (p, loc, d) -> d, (p, flip loc), p::ps)
end

let find_distance g =
  let d, ps =
    Pathfind.dijkstra (module SL) (module Int) (module Donut)
      ~start: ("AA", Outer) g
  in
  d-1, (* stop before portal ZZ *)
  "AA" :: (List.rev ps)

module DDonut = struct
  type space = graph
  type state = string * loc * int
  type data = (string * int) list
  type weight = int

  let data_id = []

  let is_end _ ((p, _, l), _) =
    p = "ZZ" && l = -1

  let neighbors g ((p, o, l), ps) =
    M.find (p, o) g
    |> List.filter (fun (p, loc, _) ->
        match loc with
        | Inner -> true
        | Outer ->
          let lp = l > 0
          and pp = p = "AA" || p = "ZZ" in
          lp && not pp || not lp && pp) (* xor *)
    |> List.map (fun (p, loc, d) ->
        let l = l + match loc with Inner -> 1 | Outer -> -1 in
        d, (p, flip loc, l), (p, l)::ps)
end

let find_distance_p2 g =
  let d, ps =
    let module SI = struct
      type t = string * loc * int
      let compare (a, l1, b) (c, l2, d) =
        match String.compare a c with
        | 0 -> begin
            match Int.compare b d with
            | 0 -> compare_loc l1 l2
            | v -> v
          end
        | v -> v
    end
    in
    Pathfind.dijkstra (module SI) (module Int) (module DDonut)
      ~start: ("AA", Outer, 0) g
  in
  d-1, (* stop before portal ZZ *)
  ("AA", 0) :: (List.rev ps) (* debug output *)

let () =
  let data = IO.read_lines () |> Map.parse |> Map.join_portals in
  let g =
    make_graph data
    (* adding empty edge to prevent error *)
    |> M.add ("AA", Inner) []
  in
  begin
    (* PART 1 *)
    g |> find_distance |> fst |> Printf.printf "%d\n";

    (* PART 2 *)
    g |> find_distance_p2 |> fst |> Printf.printf "%d\n";
  end
