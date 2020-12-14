type map_sign = Open | Tree

exception Not_a_sign

let of_char = function
  | '.' -> Open
  | '#' -> Tree
  | _ -> raise Not_a_sign

(* Map is two-dimension array of map_sign, whose top left position is
 * (0, 0) and goes down right. This map is transposed; you should use
 * map.(y).(x) to access to position x, y.
 *)
type map = map_sign array array

(* maps functions *)
let height = Array.length
let width map = Array.length map.(0)

let read_map file =
    Util.read_lines file
    |> List.map (fun line ->
        String.to_seq line
        |> Seq.map of_char
        |> Array.of_seq)
    |> Array.of_list

(* x, y *)
type pos = int * int

(* right 3, down 1 *)
let slide_angle = (3, 1)

let next_pos (x, y) (down, right) width =
  ((x + right) mod width), y + down

let slide (angle: pos) (map: map) : map_sign list =
  let rec inner (x, y) =
   if y >= (height map) then []
   else
     let s = map.(y).(x)
     and next_pos = next_pos (x, y) angle (width map)
     in s::(inner next_pos)
  in inner (0, 0)

let count_thuds map angle =
  slide angle map
  |> List.filter ((=) Tree)
  |> List.length

let main path =
  let map = open_in path |> read_map in
  begin
    (* PART 1 *)
    count_thuds map slide_angle
    |> print_int;

    print_newline ();

    (* PART 2 *)
    List.map (count_thuds map)
      [ 1, 1; 1, 3; 1, 5; 1, 7; 2, 1;]
    |> List.fold_left Int.mul 1
    |> print_int
  end

let _ = Arg.parse [] main ""
