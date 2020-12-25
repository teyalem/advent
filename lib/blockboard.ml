open Block
open Cellautomata

module type SignCellType = sig
  include SignType
  include CellType with type t := t
end

(* 2D Position *)
module Position2D = struct
  type t = int * int

  let neighbor_positions (x, y) =
    let all_neighs =
      [ -1, -1; 0, -1; 1, -1;
        -1,  0;        1,  0;
        -1,  1; 0,  1; 1,  1 ]
    in
    List.map (fun (dx, dy) -> x + dx, y + dy) all_neighs

end

module Make (B: Block.S)
: (BoardType with type t = B.t
              and type elt = B.elt
              and type pos = int * int) =
struct
  type t = B.t
  type elt = B.elt
  type pos = int * int

  let get_cell b (x, y) =
    try Some (B.get b x y)
    with _ -> None

  let set_cell b (x, y) elt = B.set b x y elt

  let iteri_cell f b =
    B.iteri (fun x y c -> f (x, y) c) b
end
