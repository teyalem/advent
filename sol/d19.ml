open Ut

module Pos = struct
  type t = int * int * int

  let make x y z = x, y, z

  let compare (a, b, c) (x, y, z) =
    match Int.compare a x with
    | 0 -> begin match Int.compare b y with
        | 0 -> Int.compare c z
        | v -> v
      end
    | v -> v

  type rotation = int * int * int -> int * int * int

  (* dumb hardcoding *)
  let rotations = [
    (fun (x, y, z) -> x, y, z);
    (fun (x, y, z) -> x, z, -y);
    (fun (x, y, z) -> x, -y, -z);
    (fun (x, y, z) -> x, -z, y);
    (fun (x, y, z) -> -x, -y, z);
    (fun (x, y, z) -> -x, z, y);
    (fun (x, y, z) -> -x, y, -z);
    (fun (x, y, z) -> -x, -z, -y);
    (fun (x, y, z) -> y, -x, z);
    (fun (x, y, z) -> y, z, x);
    (fun (x, y, z) -> y, x, -z);
    (fun (x, y, z) -> y, -z, -x);
    (fun (x, y, z) -> -y, x, z);
    (fun (x, y, z) -> -y, z, -x);
    (fun (x, y, z) -> -y, -x, -z);
    (fun (x, y, z) -> -y, -z, x);
    (fun (x, y, z) -> z, y, -x);
    (fun (x, y, z) -> z, -x, -y);
    (fun (x, y, z) -> z, -y, x);
    (fun (x, y, z) -> z, x, y);
    (fun (x, y, z) -> -z, -x, y);
    (fun (x, y, z) -> -z, y, x);
    (fun (x, y, z) -> -z, x, -y);
    (fun (x, y, z) -> -z, -y, -x);
  ]

  let apply f p = f p

  let add (a, b, c) (x, y, z) =
    a+x, b+y, c+z

  let sub (a, b, c) (x, y, z) =
    a-x, b-y, c-z

  let manhattan a b =
    let x, y, z = sub a b in
    abs x + abs y + abs z
end

module PS = Set.Make(Pos)

let parse str =
  Delim.split_line str
  |> List.tl
  |> List.map (fun s -> Scanf.sscanf s "%d,%d,%d" Pos.make)
  |> PS.of_list

(* debug *)
let print bs =
  let p = fun (x, y, z) -> Printf.printf "%d,%d,%d\n" x y z in
  PS.iter p bs

let apply o bs =
  PS.map (fun p -> Pos.add p o) bs

let find_relpos s0 bs : Pos.t option =
  let origin o = PS.map (fun x -> Pos.sub x o) in
  PS.elements s0
  |> List.find_map (fun p1 ->
      let s0 = origin p1 s0 in
      PS.elements bs
      |> List.find_opt (fun p2 ->
          let n = origin p2 bs |> PS.inter s0 |> PS.cardinal in
          n >= 12)
      |> (function
          | None -> None
          | Some p2 -> Some (Pos.sub p1 p2)))

let find_rot_and_pos s0 maps : (PS.t * Pos.t) option =
  maps
  |> List.find_map (fun (rot, bs) ->
      match find_relpos s0 bs with
      | None -> None
      | Some p -> Some (bs, p))

let rec merge_areas ps s0 sl =
  let pass, rest = sl |> List.partition_map (fun bs ->
      match find_rot_and_pos s0 bs with
      | None -> Right bs
      | Some (bs, p) -> Left (p, apply p bs))
  in
  let nps, pass = List.split pass in
  let s0 = List.fold_left PS.union s0 pass in
  if pass = [] && rest <> [] then assert false
  else if rest = []
  then ps @ nps, s0
  else merge_areas (ps @ nps) s0 rest

let largest_manhattan ps =
  List.concat_map
    (fun p -> List.map (Pos.manhattan p) ps)
    ps
  |> List.fold_left max Int.min_int

let () =
  let data = IO.read_all () |> Delim.split "\n\n" |> List.map parse in
  begin
    let s0 = List.hd data
    and sl = List.tl data in
    let sl =
      List.map (fun bs ->
          List.map
            (fun rot -> rot, PS.map (Pos.apply rot) bs)
            Pos.rotations)
        sl
    in

    let ps, map = merge_areas [] s0 sl in

    (* PART 1 *)
    PS.cardinal map |> Printf.printf "%d\n";

    (* PART 2 *)
    largest_manhattan ps |> Printf.printf "%d\n";
  end
