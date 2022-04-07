open Ut

module Tile = struct
  type t = Empty | Floor | Wall | Oxygen

  let default = Empty

  let to_char = function
      Empty -> '.'
    | Floor -> '_'
    | Wall -> '#'
    | Oxygen -> 'O'

  let of_char _ = assert false

  let next neighs c = 
    if c = Floor && List.exists ((=) Oxygen) neighs
    then Oxygen
    else c
end

module Pos = struct
  type t = int * int

  (* north, south, west, east *)
  let all_moves = [ 1; 2; 3; 4 ]
  let all_poses = [ 0, -1; 0, 1; -1, 0; 1, 0 ]

  let neighbor_positions (bx, by) = 
    List.map (fun (x, y) -> bx + x, by + y) all_poses

  let of_dir move = List.nth all_poses (move - 1)

end

module Map = struct

  module B = Block.Make(Tile)
  module Board = BlockBoard.Make(B)

  include B
  include Board
  include CellularAutomata.Make(Board)(Pos)(Tile)

  (* handy function *)
  let get_rel b x y = get b (dimx b/2 + x) (dimy b/2 + y)
  let set_rel b x y c = set b (dimx b/2 + x) (dimy b/2 + y) c

end

module Bot = struct
  type t = { mutable found: bool;
             mutable x: int;
             mutable y: int;
             mutable dis: int;
             vm: IntCode.t; }

  let init code = { found = false;
                    x = 0;
                    y = 0;
                    dis = 0;
                    vm = IntCode.load code; }

  let copy b = { found = b.found;
                 x = b.x;
                 y = b.y;
                 dis = b.dis;
                 vm = IntCode.copy b.vm; }

  let run b d =
    let x, y = Pos.of_dir d in
    let x = b.x + x
    and y = b.y + y in
    IntCode.set_input b.vm d;
    IntCode.run b.vm;
    b.dis <- b.dis + 1;
    match IntCode.get_output b.vm with
      0 -> x, y, Tile.Wall
    | 1 ->
      b.x <- x;
      b.y <- y;
      x, y, Tile.Floor

    | 2 ->
      b.found <- true;
      x, y, Tile.Oxygen

    | n -> failwith (Printf.sprintf "invalid output: %d" n)

  let explore code =
    let map = Map.make 50 50 in
    let bot = init code in

    let distance = ref 0 in

    let rec bfs = function
        [] -> ()
      | bot :: rest ->
        let bots =
          Pos.all_moves
          |> List.filter (fun d ->
              let x, y = Pos.of_dir d in
              Map.get_rel map (bot.x + x) (bot.y + y) = Tile.Empty)

          |> List.filter_map (fun d ->
              let b = copy bot in
              let x, y, t = run b d in
              Map.set_rel map x y t;
              if t <> Tile.Wall
              then Some b
              else None)
        in

        begin
          match List.find_opt (fun b -> b.found) bots with
            None -> ()
          | Some b -> distance := b.dis
        end;
        bfs (rest @ bots)
    in
    bfs [bot];
    !distance, map

end

let fill_oxygen map =
  let rec loop i =
    let map = Map.next_state map in
    if Map.count_occur Tile.Floor map = 0
    then i
    else loop (i+1)
  in
  loop 1

let main path =
  let data = open_in path |> IO.read_file |> IntCode.parse_code in
  begin
    (* PART 1 *)
    let dis, map = Bot.explore data in
    Printf.printf "%d\n" dis;

    (* PART 2 *)
    fill_oxygen map |> Printf.printf "%d\n"
  end

let () = Arg.parse [] main ""
