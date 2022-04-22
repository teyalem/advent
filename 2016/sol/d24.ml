open Ut

module T = struct
  type t = Wall
         | Empty
         | POI of int

  let default = Empty

  let of_char = function
    | '#' -> Wall
    | '.' -> Empty
    | c when '0' <= c && c <= '9' -> POI Char.(code c - code '0')
    | _ -> assert false

  let to_char = function
    | Wall -> '#'
    | Empty -> '.'
    | POI n -> Char.(chr @@ n + code '0')

  let is_wall = function
    | Wall -> true
    | _ -> false

  let poi = function
    | POI n -> Some n
    | _ -> None
end

module Maze = Block.Make(T)

module S = Set.Make(Int)

module Bot = struct
  type space = Maze.t * int * (int * int) * bool
  type state = (int * int) * S.t
  type data = (int * int) list
  type weight = int

  let data_id = []

  let is_end (_, n, startpos, part2) ((pos, set), _) =
    S.cardinal set = n && (not part2 || pos = startpos)

  let neighbors (maze, _, _, _) ((pos, set), ds) =
    Neigh.(neighbors von_neumann pos)
    |> List.filter (fun (x, y) ->
        0 <= x && x < Maze.dimx maze && 0 <= y && y < Maze.dimy maze)
    |> List.filter (fun (x, y) -> Maze.get maze x y |> T.is_wall |> not)
    |> List.map (fun (x, y as pos) ->
        let t = Maze.get maze x y in
        let s = S.add_seq (Option.to_seq @@ T.poi t) set in
        1, (pos, s), pos::ds)
end

module C = struct
  type t = Bot.state
  let compare = Stdlib.compare
end

(* quite slow, but lazyness pulls me from improving it. *)
let solve n start maze part2 =
  Pathfind.dijkstra (module C) (module Int) (module Bot)
    ~start: (start, S.singleton 0)
    (maze, n, start, part2)

let () =
  let data = IO.read_lines () |> Maze.parse in
  let start = ref (0, 0) in
  Maze.iteri (fun x y t -> if t = POI 0 then start := (x, y)) data;

  let nnum =
    Maze.to_matrix data
    |> Mat.to_seq
    |> Seq.concat
    |> Seq.filter (function T.POI _ -> true | _ -> false)
    |> Seq.length
  in
  let solve part2 = solve nnum !start data part2 in

  (* PART 1 *) solve false |> fst |> Printf.printf "%d\n";
  (* PART 2 *) solve true |> fst |> Printf.printf "%d\n";
