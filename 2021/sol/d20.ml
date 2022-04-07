open Ut

module P = struct
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

module B = Block.Make(P)

(* convert a list of pixels to an int *)
let pixels_to_int ts =
  List.map P.to_int ts
  |> List.fold_left (fun p n -> 2*p + n) 0

let parse = function
  | alg :: "" :: img ->
    let alg = String.to_seq alg |> Seq.map P.of_char |> Array.of_seq in
    alg, B.parse img
  | _ -> assert false

(* relative postitions of pixels to consider to determine a pixel *)
let ps =
  let ps = [ -1; 0; 1 ] in
  List.concat_map (fun y -> List.map (fun x -> x, y) ps) ps

(* get index of pixel that is going to be in position (x, y) *)
let get_idx dp img x y =
  (* out-of-bounds pixels are dp. *)
  let g x y = try B.get img x y with _ -> dp in
  (* because center part of new image is expanded by 2 pixels
   * horizontally and vertically, we need to look at position (x-1, y-1). *)
  List.map (fun (dx, dy) -> x-1 + dx, y-1 + dy) ps
  |> List.map (fun (x, y) -> g x y)
  |> pixels_to_int

(* apply enhancing algorithm to image, assuming pixels not noted are dp. *)
let apply alg dp img : B.t =
  Mat.init (B.dimx img + 2) (B.dimy img + 2)
    (fun x y -> alg.(get_idx dp img x y))

(* apply enhance algorithm n times. *)
let enhance n alg img =
  let rec aux i dp img =
    if i = n then img
    else
      (* default pixel is determined by previous default pixel. *)
      let ai = match dp with P.Dark -> 0 | P.Light -> 511 in
      aux (i+1) alg.(ai) @@ apply alg dp img
  in
  (* original default pixel is dark. *)
  aux 0 P.Dark img

let () =
  let alg, img = IO.read_lines () |> parse in
  let f n =
    enhance n alg img |> B.count_occur P.Light |> Printf.printf "%d\n"
  in
  begin
    (* PART 1 *) f 2;
    (* PART 2 *) f 50;
  end
