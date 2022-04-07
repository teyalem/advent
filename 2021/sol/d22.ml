open Ut

type state = On | Off
type range = int * int (* l, h. l <= h. if l > h then it's range of no elements. *)
type range3d = range * range * range
type cuboid = (range3d, int ref) Hashtbl.t

let state_of_string = function
  | "on" -> On
  | "off" -> Off
  | _ -> assert false

let parse str =
  Scanf.sscanf str "%s x=%d..%d,y=%d..%d,z=%d..%d"
    (fun s xl xh yl yh zl zh ->
       state_of_string s, ((xl, xh), (yl, yh), (zl, zh)))

let init_range (l, h) =
  -50 <= l && h <= 50

let is_zero (l, h) =
  l > h

let is_zero3d (xr, yr, zr) =
  is_zero xr || is_zero yr || is_zero zr

let overlap (al, ah) (bl, bh) =
  max al bl, min ah bh

let overlap3d (xr, yr, zr) (ar, br, cr) =
  overlap xr ar, overlap yr br, overlap zr cr

let add cube (s, r) =
  let aux (k, n) =
    match Hashtbl.find_opt cube k with
    | None -> Hashtbl.add cube k @@ ref n
    | Some v -> v := !v + n
  in
  let update = ref [] in
  cube
  |> Hashtbl.iter (fun kr n ->
      let nr = overlap3d r kr in
      if not @@ is_zero3d nr then
        update := (nr, ~- !n) :: !update);
  if s = On then update := (r, 1) :: !update;
  List.iter aux !update;
  cube

let volume (xr, yr, zr) =
  let len (l, h) = h - l + 1 in
  len xr * len yr * len zr

let count_ons cube =
  Hashtbl.fold (fun k v acc -> acc + !v * volume k) cube 0

let () =
  let data = IO.read_lines () |> List.map parse in
  begin
    (* PART 1 *)
    data
    |> List.filter (fun (_, (xr, yr, zr)) ->
        init_range xr && init_range yr && init_range zr)
    |> List.fold_left add (Hashtbl.create 100)
    |> count_ons
    |> Printf.printf "%d\n";

    (* PART 2 *)
    List.fold_left add (Hashtbl.create 100) data
    |> count_ons
    |> Printf.printf "%d\n";
  end
