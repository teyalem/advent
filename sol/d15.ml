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

module M = Map.Make(Int)

let neigh = [ 0, 1; 0, -1; -1, 0; 1, 0 ]

(* find lowest risk by dijkstra's method. *)
let lowest_risk b =
  let dimx = B.dimx b and dimy = B.dimy b in
  let visited = Array.make_matrix dimx dimy false in

  let q = ref M.empty in
  let add w x y =
    match M.find_opt w !q with
    | None -> q := M.add w (ref [x, y]) !q
    | Some l -> l := (x, y) :: !l
  in
  let rec next () =
    let w, l = M.min_binding !q in
    match !l with
    | [] -> q := M.remove w !q; next ()
    | (x, y)::ps -> l := ps; w, x, y
  in

  (* dijkstra loop *)
  let rec aux (w, x, y) =
    if x = dimx-1 && y = dimy-1 then w
    else if visited.(x).(y) then aux @@ next ()
    else begin
      visited.(x).(y) <- true;
      List.map (fun (dx, dy) -> x+dx, y+dy) neigh
      |> List.filter (fun (x, y) ->
          0 <= x && x < dimx && 0 <= y && y < dimy)
      |> List.iter (fun (x, y) ->
          let w = B.get b x y + w in add w x y);
        aux @@ next ()
    end
  in
  aux (0, 0, 0)

let enlarge b =
  let inc b = Mat.map (fun e -> if e = 9 then 1 else e+1) b in
  let t =
    List.init 10 (fun i -> i)
    |> List.fold_left
      (fun a i -> if i > 0 then a.(i) <- inc a.(i-1); a)
      (Array.make 10 b)
  in
  List.init 5 (fun i -> List.init 5 (fun j -> i+j))
  |> List.map (List.map (fun i -> t.(i)))
  |> Mat.concat

let () =
  let data = IO.read_lines () |> B.parse in
  let solve d = lowest_risk d |> Printf.printf "%d\n" in
  begin
    (* PART 1 *) solve data;
    (* PART 2 *) solve @@ enlarge data;
  end
