open Advent

module Tile = struct
  type t = char
  let default = '.'
  let of_char c = c
  let to_char c = c
end

module Map = struct
  module B = Block.Make(Tile)

  include B
  include BlockBoard.Make(B)

  let find_entity f map =
    let pos = ref (0, 0) in
    iteri (fun x y c -> if f c then pos := (x, y) else ()) map;
    !pos

  let find_all f map =
    let result = ref [] in
    iteri (fun x y c -> if f c then result := (x, y) :: !result) map;
    !result

end

let dir = [ 0, -1; 1, 0; 0, 1; -1, 0 ]

let is_lowercase c = 'a' <= c && c <= 'z'
let is_uppercase c = 'A' <= c && c <= 'Z'

let either a b = fun c -> a c || b c

let is_alphabet = either is_lowercase is_uppercase

let to_uppercase c =
  if is_lowercase c
  then Char.(code c - code 'a' + code 'A' |> chr)
  else c

let to_lowercase c =
  if is_uppercase c
  then Char.(code c - code 'A' + code 'a' |> chr)
  else c

let group_by (f: 'a -> 'b) (l: 'a list) : 'a list list =
  List.fold_right (fun n (prev, p) ->
      match prev with
        None -> Some [n], p
      | Some l ->
        if f n = f (List.hd l)
        then Some (n::l), p
        else Some [n], l::p)
    l
    (None, [])
  |> (fun (p, l) -> (Option.get p)::l)

let collect_dests map start =
  let q = Queue.create () in
  let rec bfs () =
    if Queue.is_empty q
    then []
    else
      let pos, dis = Queue.pop q in
      let t = Map.get_cell map pos |> Option.get in

      if is_alphabet t || t = '@'
      then (pos, t, dis) :: bfs ()
      else if t = '#'
      then bfs ()

      else begin
        Map.set_cell map pos '#';
        List.map (Coord.add pos) dir
        |> List.filter (fun pos -> Map.get_cell map pos <> (Some '#'))
        |> List.iter (fun pos -> Queue.push (pos, dis+1) q);
        bfs ()
      end
  in

  Map.set_cell map start '.';
  Queue.push (start, 0) q;
  bfs ()

let find_starting_point map =
  Map.find_entity ((=) '@') map

let find_keys map =
  Map.find_all is_lowercase map
  |> List.map (fun (x, y) -> (x, y), Map.get map x y)

let find_doors map =
  Map.find_all is_uppercase map
  |> List.map (fun (x, y) -> (x, y), Map.get map x y)

let make_graph map : (char, (char * int) list) Hashtbl.t =
  let start = find_starting_point map
  and keys = find_keys map
  and doors = find_doors map in

  List.map (fun (pos, c) ->
      c, collect_dests (Map.copy map) pos
         |> List.map (fun (_, t, dis) -> t, dis)
         |> List.sort (fun a b -> compare (fst a) (fst b))

         |> group_by fst
         |> List.map (fun l ->
             List.(fold_left
                     (fun (_, pdis) (t, dis) ->
                        if pdis < dis then t, pdis else t, dis)
                     (hd l)
                     (tl l))))
    ((start, '@') :: keys @ doors)
  |> List.to_seq
  |> Hashtbl.of_seq

(* implementing dijkstra's algorithm *)
let find_shortest_route graph keyn start = begin

end

let print_graph graph = begin
  Printf.printf "digraph G {\n";
  Hashtbl.iter (fun k v ->
      List.iter (fun (t, _) -> Printf.printf "\"%c\" -> \"%c\"\n" k t) v)
    graph;
  Printf.printf "}";
end

let main path =
  let data = open_in path |> IO.read_lines |> Map.parse in
  begin
    (* PART 1 *)
    let graph = make_graph data in

  end

let () = Arg.parse [] main ""
