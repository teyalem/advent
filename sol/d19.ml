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

let origin o = PS.map (fun x -> Pos.sub x o)

let apply o bs =
  PS.map (fun p -> Pos.add p o) bs

let find_relpos maps bs : Pos.t option =
  maps
  |> List.find_map (fun (p1, s0) ->
      PS.elements bs
      |> List.find_opt (fun p2 ->
          let n = origin p2 bs |> PS.inter s0 |> PS.cardinal in
          n >= 12)
      |> Option.map (fun p2 -> Pos.sub p1 p2))

let find_rot_and_pos maps rbs : (PS.t * Pos.t) option =
  rbs
  |> List.find_map (fun (rot, bs) ->
      match find_relpos maps bs with
      | None -> None
      | Some p -> Some (bs, p))

let rec merge_areas ps map sl =
  let maps =
    PS.elements map
    |> List.map (fun p -> p, origin p map)
  in
  let pass, rest = sl |> List.partition_map (fun bs ->
      match find_rot_and_pos maps bs with
      | None -> Right bs
      | Some (bs, p) -> Left (p, apply p bs))
  in
  let nps, pass = List.split pass in
  let map = List.fold_left PS.union map pass in
  if pass = [] && rest <> [] then assert false
  else if rest = []
  then ps @ nps, map
  else merge_areas (ps @ nps) map rest

let largest_manhattan ps =
  List.concat_map
    (fun p -> List.map (Pos.manhattan p) ps)
    ps
  |> List.fold_left max Int.min_int

(* it takes approx. 1m 30s. *)
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
