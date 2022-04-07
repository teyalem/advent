open Ut

module MapSign = struct
  type t = Open | Tree

  let default = Open

  exception Not_a_sign

  let of_char = function
    | '.' -> Open
    | '#' -> Tree
    | _ -> raise Not_a_sign

  let to_char = function
    | Open -> '.'
    | Tree -> '#'

end

module Map = struct

  include Block.Make(MapSign)

  let next_pos (x, y) (right, down) width =
    ((x + right) mod width), y + down

  (* slide through map *)
  let slide angle map : elt list =
    let rec inner (x, y) =
      if y >= (dimy map) then []
      else
        let s = get map x y
        and next_pos = next_pos (x, y) angle (dimx map)
        in s::(inner next_pos)
    in inner (0, 0)

  let count_thuds map angle =
    slide angle map
    |> List.filter ((=) MapSign.Tree)
    |> List.length

end

(* right 3, down 1 *)
let slide_angle = (3, 1)

let main path =
  let map = open_in path |> IO.input_lines |> Map.parse in
  begin
    (* PART 1 *)
    Map.count_thuds map slide_angle
    |> print_int;

    print_newline ();

    (* PART 2 *)
    List.map (Map.count_thuds map)
      [ 1, 1; 3, 1; 5, 1; 7, 1; 1, 2;]
    |> sum
    |> print_int

  end

let _ = Arg.parse [] main ""
