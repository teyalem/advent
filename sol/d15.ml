open Ut

module I = struct
  type t = int
  let default = 0
  let of_char c = Char.(code c - code '0')
  let to_char c = assert false
end

module B = Block.Make(I)

module IP = struct
  type t = int * int * int (* w, x, y *)
  let compare (w, a, b) (v, c, d) =
    match compare w v with
    | 0 ->
      begin match compare a c with
        | 0 -> compare b d
        | v -> v
      end
    | v -> v
end

module Set = Set.Make(IP)

let neigh = [ 0, 1; 0, -1; -1, 0; 1, 0 ]

(* find lowest risk by dijkstra's method. *)
let lowest_risk b =
  let dimx = B.dimx b and dimy = B.dimy b in
  let d = Array.make_matrix dimx dimy Int.max_int in
  d.(0).(0) <- 0;
  let visited = Array.make_matrix dimx dimy false in

  let nodes = ref Set.empty in
  let add x y = nodes := Set.add (d.(x).(y), x, y) !nodes in
  let next_point () =
    let (w, x, y) = Set.min_elt !nodes in
    nodes := Set.remove (w, x, y) !nodes;
    x, y
  in

  let rec aux (x, y) =
    visited.(x).(y) <- true;
    let w = d.(x).(y) in
    List.map (fun (dx, dy) -> x+dx, y+dy) neigh
    |> List.filter (fun (x, y) ->
        0 <= x && x < dimx && 0 <= y && y < dimy)
    |> List.iter (fun (x, y) ->
        let w = B.get b x y + w in
        if w < d.(x).(y) then d.(x).(y) <- w; (* relaxing *)
        if not visited.(x).(y) then add x y
      );

    if x <> dimx-1 || y <> dimy-1
    then aux @@ next_point ()
  in
  aux (0, 0);
  d.(dimx-1).(dimy-1)

let enlarge b =
  let inc b =
    B.to_matrix b
    |> Array.map (fun a ->
        Array.map (fun e -> if e = 9 then 1 else e+1) a)
    |> B.of_matrix
  in

  let concat_horiz bs =
    List.map B.to_matrix bs
    |> Array.concat
    |> B.of_matrix

  and concat_vert bs =
    let dimy = B.dimy @@ List.hd bs in
    let bs = List.map B.to_matrix bs in
    Array.init dimy
      (fun i -> List.map (fun b -> b.(i)) bs)
    |> Array.map Array.concat
    |> B.of_matrix
  in

  let t =
    List.init 10 (fun i -> i)
    |> List.fold_left
      (fun a i ->
         if i = 0 then a.(i) <- b else a.(i) <- inc a.(i-1); a)
      (Array.make 10 b)
  in
  List.init 5 (fun i -> List.init 5 (fun j -> i+j))
  |> List.map (List.map (fun i -> t.(i)))
  |> List.map concat_vert
  |> concat_horiz

let () =
  let data = IO.read_lines () |> B.parse in
  begin
    (* PART 1 *)
    lowest_risk data
    |> Printf.printf "%d\n";

    (* PART 2 *)
    enlarge data
    |> lowest_risk
    |> Printf.printf "%d\n";
  end
