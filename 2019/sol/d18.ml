open Ut

let is_lowercase c = 'a' <= c && c <= 'z'
let is_uppercase c = 'A' <= c && c <= 'Z'

module C = Set.Make(Char)
module P = Pheap.Make(Int)

module T = struct
  type t = char
  let default = '.'
  let of_char c = c
  let to_char c = c
end

module Map = struct
  include Block.Make(T)

  let find_pos f map =
    let pos = ref (0, 0) in
    try
      map |> iteri (fun x y c ->
          if f c then begin
            pos := (x, y); raise Exit
          end);
      raise Not_found
    with Exit -> !pos

  let find_all f map =
    let result = ref [] in
    iteri (fun x y c -> if f c then result := (x, y) :: !result) map;
    !result
end

module K : Pathfind.StateSpace
  with type space = Map.t
   and type state = int * int
   and type data = char * int * C.t =
struct
  type space = Map.t
  type state = int * int
  type data = char * int * C.t

  let data_id = '_', 0, C.empty

  let is_end _ (_, (t, d, _)) =
    d <> 0 && is_lowercase t || t = '@'

  let neighbors m (pos, (_, d, doors)) =
    Neigh.(neighbors von_neumann pos)
    |> List.filter (fun (x, y) ->
        0 <= x && x < Mat.dimx m && 0 <= y && y < Mat.dimy m)
    |> List.filter (fun (x, y) -> Mat.get m x y <> '#')
    |> List.map (fun (x, y) ->
        let t = Mat.get m x y in
        let doors = if is_uppercase t then C.add t doors else doors in
        (x, y), (t, d+1, doors))
end

type graph = (char, (char * int * C.t) list) Hashtbl.t

let make_graph f map =
  let module IP = struct type t = int * int let compare = Stdlib.compare end in
  let entrances = Map.find_all f map
  and keys = Map.find_all is_lowercase map in
  entrances @ keys
  |> List.map (fun (x, y) ->
      Map.get map x y,
      Pathfind.bfs_collect (module IP) (module K) ~start: (x, y) map)
  |> List.to_seq
  |> Hashtbl.of_seq

let all_keys = ref C.empty

module G : Pathfind.WeightedGraph
  with type space = graph
   and type state = char * C.t
   and type data = char list
   and type weight = int =
struct
  type space = graph
  type state = char * C.t
  type data = char list
  type weight = int

  let data_id = []

  let is_end _ ((_, ks), _) =
    C.equal ks !all_keys

  let neighbors g ((k, ks), pos) =
    Hashtbl.find g k
    |> List.filter (fun (_, _, ds) ->
        C.(subset (map Char.lowercase_ascii ds) ks))
    |> List.map (fun (c, d, _) ->
        let ks = if is_lowercase c then C.add c ks else ks in
        d, (c, ks), c::pos)
end

(* Dijkstra *)
let find_way_p1 graph =
  let module State = struct
    type t = char * C.t
    let compare (a, b) (c, d) =
      match Char.compare a c with
      | 0 -> C.compare b d
      | v -> v
  end
  in
  let module S = Set.Make(State) in
  let q = ref @@ P.empty in
  let visited = ref S.empty in
  let add (w, st, ps) = q := P.insert w (st, ps) !q in
  let rec next () =
    let w, ((c, ks), ps) = P.find_min !q in
    q := P.delete_min !q;
    if S.mem (c, ks) !visited
    then next ()
    else w, c, ks, ps
  in
  let rec aux (w, c, ks, ps) =
    if G.is_end graph ((c, ks), ps) then w, ps
    else begin
      visited := S.add (c, ks) !visited;
      G.neighbors graph ((c, ks), ps)
      |> List.map (fun (cost, st, ps) -> w + cost, st, ps)
      |> List.iter add;
      aux @@ next ()
    end
  in
  let w, _ = aux (0, '@', C.empty, []) in
  w

module G2 : Pathfind.WeightedGraph
  with type space = graph
   and type state = char list * C.t
   and type data = (int * char) list
   and type weight = int =
struct
  type space = graph
  type state = char list * C.t
  type data = (int * char) list
  type weight = int

  let data_id = []

  let is_end _ ((_, ks), _) =
    C.equal ks !all_keys

  let neighbors g ((ps, ks), ms) =
    ps
    |> List.mapi (fun i k ->
        Hashtbl.find g k
        |> List.filter (fun (_, _, ds) ->
            C.(subset (map Char.lowercase_ascii ds) ks))
        |> List.map (fun (c, d, _) ->
            let ks = if is_lowercase c then C.add c ks else ks in
            i, d, c, ks, (i, c)::ms))
    |> List.concat_map (List.map (fun (i, d, c, ks, ms) ->
        let ps = List.mapi (fun j x -> if j = i then c else x) ps in
        d, (ps, ks), ms))
end

(* Dijkstra *)
let find_way_p2 graph =
  let module State = struct
    type t = char list * C.t
    let compare (a, b) (c, d) =
      match List.compare Char.compare a c with
      | 0 -> C.compare b d
      | v -> v
  end
  in
  let module S = Set.Make(State) in
  let q = ref @@ P.empty in
  let visited = ref S.empty in
  let add (w, st, ps) = q := P.insert w (st, ps) !q in
  let rec next () =
    let w, ((ps, ks), ms) = P.find_min !q in
    q := P.delete_min !q;
    if S.mem (ps, ks) !visited
    then next ()
    else w, ps, ks, ms
  in
  let rec aux (w, ps, ks, ms) =
    if G2.is_end graph ((ps, ks), ms) then w, ms
    else begin
      visited := S.add (ps, ks) !visited;
      G2.neighbors graph ((ps, ks), ms)
      |> List.map (fun (cost, st, ms) -> w + cost, st, ms)
      |> List.iter add;
      aux @@ next ()
    end
  in
  let w, _ = aux (0, ['1'; '2'; '3'; '4'], C.empty, []) in
  w

let modify map =
  let x, y = Map.find_pos ((=) '@') map in
  [[ '1'; '#'; '2' ];
   [ '#'; '#'; '#' ];
   [ '3'; '#'; '4' ];]
  |> List.iteri (fun dy l ->
      l |> List.iteri (fun dx c ->
          Map.set map (x-1+dx) (y-1+dy) c))

let () =
  let data = IO.read_lines () |> Map.parse in
  begin
    all_keys :=
      Map.find_all is_lowercase data
      |> List.map (fun (x, y) -> Map.get data x y)
      |> C.of_list;

    (* PART 1 *)
    let g = make_graph ((=) '@') data in
    g |> find_way_p1 |> Printf.printf "%d\n";

    (* PART 2 *)
    modify data;
    let is_ent c = '1' <= c && c <= '4' in
    let g = make_graph is_ent data in
    g |> find_way_p2 |> Printf.printf "%d\n";
  end
