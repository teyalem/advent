open Ut

module Tile = struct
  type t = Dark | Light

  let default = Dark
  let of_char = function
    | '.' -> Dark
    | '#' -> Light
    | _ -> assert false

  let to_char = function
    | Dark -> '.'
    | Light -> '#'

  let to_int = function
    | Dark -> 0
    | Light -> 1
end

module B = Block.Make(Tile)

let tiles_to_int ts =
  List.map Tile.to_int ts
  |> List.fold_left (fun p n -> 2*p + n) 0

let parse = function
  | alg :: "" :: img ->
    let alg =
      String.to_seq alg
      |> Seq.map Tile.of_char
      |> Array.of_seq
    in
    alg, B.parse img
  | _ -> assert false

let ns = [ -1; 0; 1 ]
let ns = List.concat_map (fun y -> List.map (fun x -> x, y) ns) ns

(* enlarged size *)
let el = 2 (* 2 is enough *)

let get_idx dt img x y =
  let g x y = try B.get img x y with _ -> dt in
  List.map (fun (dx, dy) -> x - el + dx, y - el + dy) ns
  |> List.map (fun (x, y) -> g x y)
  |> tiles_to_int

let apply dt alg img : B.t =
  Mat.init (B.dimx img + el*2) (B.dimy img + el*2)
    (fun x y -> alg.(get_idx dt img x y))

let enhance n alg img =
  let rec aux i dt img =
    if i = n then img
    else
      let ndt = alg.(tiles_to_int @@ List.init 9 @@ Fun.const dt) in
      aux (i+1) ndt @@ apply dt alg img
  in
  aux 0 Tile.Dark img

let () =
  let alg, img = IO.read_lines () |> parse in
  begin
    let f n =
      enhance n alg img
      |> B.count_occur Tile.Light
      |> Printf.printf "%d\n"
    in

    (* PART 1 *)
    f 2;

    (* PART 2 *)
    f 50;
  end
