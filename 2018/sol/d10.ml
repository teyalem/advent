open Ut

module IP = struct
  type t = int * int

  let compare (a, b) (c, d) =
    match Int.compare a c with
    | 0 -> Int.compare b d
    | v -> v
end

let parse str =
  Scanf.sscanf str "position=< %d,  %d> velocity=< %d,  %d>"
    (fun x y v w -> (x, y), (v, w))

let update ((x, y), (v, w)) =
  (x+v, y+w), (v, w)

let bound ps =
  let open List in
  let xs = map fst ps and ys = map snd ps in
  let xl = fold_left min max_int xs
  and xh = fold_left max min_int xs
  and yl = fold_left min max_int ys
  and yh = fold_left max min_int ys in
  xl, xh, yl, yh

let len l h = h - l + 1

let box_size (xl, xh, yl, yh) =
  (len xl xh) * (len yl yh)

let find_msg =
  let f ls = List.map fst ls |> bound |> box_size in
  let rec aux i bsize ls =
    let next = List.map update ls in
    let nsize = f next in
    if nsize > bsize then i, ls
    else aux (i+1) nsize next
  in
  fun lights -> aux 0 (f lights) lights

let print ls =
  let xl, xh, yl, yh = List.map fst ls |> bound in
  let dx = len xl xh and dy = len yl yh in
  let mat = Array.make_matrix dx dy '.' in
  List.iter (fun ((x, y), _) -> mat.(x - xl).(y - yl) <- '#') ls;
  for y = 0 to dy - 1 do
    for x = 0 to dx - 1 do
      Printf.printf "%c" mat.(x).(y)
    done;
    Printf.printf "\n"
  done

let () =
  let data = IO.read_lines () |> List.map parse in
  let i, msg = find_msg data in
  (* PART 1 *) print msg;
  (* PART 2 *) print_int i;
