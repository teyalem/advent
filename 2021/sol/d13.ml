open Ut

module Tile = struct
  type t = Empty | Dot

  let default = Empty

  let of_char = function
    | '#' -> Dot
    | '.' -> Empty
    | _ -> assert false

  let to_char = function
    | Empty -> '.'
    | Dot -> '#'

  let merge a b =
    match a with
    | Dot -> Dot
    | Empty -> b
end

module B = Block.Make(Tile)

let fold_x b x : B.t =
  let dimy = B.dimy b in
  let n = B.make x dimy in
  for i = 1 to x do
    for j = 0 to dimy-1 do
      let t = Tile.merge (B.get b (x-i) j) (B.get b (x+i) j) in
      B.set n (x-i) j t
    done
  done;
  n

let fold_y b y =
  let dimx = B.dimx b in
  let n = B.make dimx y in
  for i = 0 to dimx-1 do
    for j = 1 to y do
      let t = Tile.merge (B.get b i (y-j)) (B.get b i (y+j)) in
      B.set n i (y-j) t
    done
  done;
  n

let fold b (axis, d) =
  match axis with
  | "x" -> fold_x b d
  | "y" -> fold_y b d
  | _ -> assert false

let parse ss =
  let size = 1400 in
  let b = B.make size size in
  let rec dot = function
    | [] -> assert false
    | ""::ss -> ss
    | s::ss ->
      Scanf.sscanf s "%d,%d" (fun x y -> B.set b x y Dot);
      dot ss
  in
  let rec fold = function
    | [] -> []
    | s::ss ->
      let d = Scanf.sscanf s "fold along %s@=%d" (fun a d -> a, d) in
      d :: fold ss
  in
  b, dot ss |> fold

let () =
  let paper, inst = IO.input_lines stdin |> parse in
  begin
    (* PART 1 *)
    let b = fold paper (List.hd inst) in
    B.count_occur Tile.Dot b |> Printf.printf "%d\n";

    (* PART 2 *)
    List.fold_left fold paper inst 
    |> B.print;
  end
