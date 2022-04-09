open Ut

let rec take n l =
  match l with
  | [] -> []
  | x::xs ->
    if n = 0
    then []
    else x :: take (n-1) xs

let test_by mf f = fun a b -> f (mf a) (mf b)

let group_by f l =
  List.fold_right
    (fun n pl ->
       match pl with
       | [] -> [[n]]
       | p::pl ->
         if f n @@ List.hd p
         then (n::p)::pl
         else [n]::p::pl)
    l
    []

let rec flatten_i l =
  if List.length l = 0
  then []
  else
    let hds = List.(map hd l)
    and tls = List.(map tl l |> filter (fun l -> length l <> 0)) in
    hds @ flatten_i tls

let length_sq (x, y) = x*x + y*y

(* find angle by arctan *)
let angle (x, y) = 
  let x = float_of_int x
  and y = float_of_int ~-y in
  let a = atan2 x y in
  if a < 0.
  then a +. 2.*.Float.pi
  else a

let is_same_line a b = angle a = angle b

let is_shadowed a posl =
  a = (0, 0) || List.exists (fun b -> is_same_line a b) posl

(* Tile module for block *)
module Tile = struct
  type t = Empty | Asteroid

  let default = Empty

  let of_char c =
    match c with
    | '.' -> Empty
    | '#' -> Asteroid
    | _ -> assert false

  let to_char c =
    match c with
    | Empty -> '.'
    | Asteroid -> '#'
end

module Map = struct
  include Block.Make(Tile)

  let count_asteroid m tx ty =
    let observed = ref [] in
    m |> iteri (fun x y c ->
        if c = Tile.Empty
        then ()
        else begin
          if is_shadowed (x - tx, y - ty) !observed
          then ()
          else observed := (x - tx, y - ty) :: !observed
        end);
    List.length !observed

  let observe map =
    let result = ref [] in
    map |> iteri (fun x y c ->
        if c = Tile.Empty
        then ()
        else result := ((x, y), count_asteroid map x y) :: !result);
    !result

  let to_aster_angle (center_x, center_y) map =
    let asters = ref [] in
    map |> iteri (fun x y c ->
        if c = Tile.Empty
        then ()
        else asters := ((x, y), angle (x - center_x, y - center_y)) :: !asters);
    List.filter (fun ((x, y), _) -> not (x = center_x && y = center_y)) !asters

  let vaporize map (loc_x, loc_y) =
    let asters = to_aster_angle (loc_x, loc_y) map 
                 |> List.sort (test_by snd compare)
                 |> group_by (test_by snd (=))
                 |> List.map
                   (List.stable_sort (fun ((x1, y1), _) ((x2, y2), _) ->
                     let s1 = length_sq (x1 - loc_x, y1 - loc_y)
                     and s2 = length_sq (x2 - loc_x, y2 - loc_y) in
                     compare s1 s2))
    in
    asters

end

let () =
  let data = IO.read_lines () |> Map.parse in
  begin
    (* PART 1 *)
    let location, observable_asteroid =
      Map.observe data
      |> List.fold_left
        (fun (pl, pn) (l, n) -> if pn > n then (pl, pn) else (l, n))
        ((-1,-1), min_int)
    in
    print_int observable_asteroid;

    print_newline ();

    (* PART 2 *)
    Map.vaporize data location
    |> flatten_i
    |> (fun l -> List.nth l 199)
    |> fst
    |> (fun (x, y) -> x*100 + y)
    |> print_int;

  end
