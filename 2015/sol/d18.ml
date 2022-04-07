open Ut

module Light = struct
  type t = On | Off

  let default = Off

  let of_char = function
    | '#' -> On
    | '.' -> Off
    | _ -> assert false

  let to_char = function
    | On -> '#'
    | Off -> '.'

  let next neigh cell =
    let on = List.filter ((=) On) neigh |> List.length in
    match cell with
    | On -> if on = 2 || on = 3 then On else Off
    | Off -> if on = 3 then On else Off
end

module Grid = BlockBoard.Make(Light)

let next_step = CellularAutomata.make_automata (module Grid)

let next_step2 g =
  ignore @@ next_step g;
  [ 0, 0; 0, 99; 99, 0; 99, 99 ]
  |> List.iter (fun p -> Grid.set g p On)

let () =
  let data = IO.read_lines () |> Grid.parse in
  begin
    (* PART 1 *)
    let g = Grid.copy data in
    for _ = 1 to 100 do ignore @@ next_step g done;
    Grid.count_occur On g |> Printf.printf "%d\n";

    (* PART 2 *)
    let g = Grid.copy data in
    for _ = 1 to 100 do next_step2 g done;
    Grid.count_occur On g |> Printf.printf "%d\n";
  end
