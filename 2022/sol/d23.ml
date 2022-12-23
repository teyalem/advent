open Ut

module P = struct
  type t = int * int

  let compare (x, y) (u, v) =
    match Int.compare x u with
    | 0 -> Int.compare y v
    | n -> n
end

module S = Set.Make(P)

let parse sl =
  sl
  |> List.mapi (fun y s ->
      String.to_seq s
      |> Seq.mapi (fun x c -> (x, y), c)
      |> Seq.filter_map (fun (pos, c) ->
          if c = '#' then Some pos else None)
      |> List.of_seq)
  |> List.concat
  |> S.of_list

let add (x, y) (u, v) = x + u, y + v

module D = struct
  let n = 0, -1
  let s = 0, 1
  let w = -1, 0
  let e = 1, 0
  let ne = add n e
  let nw = add n w
  let se = add s e
  let sw = add s w

  let directions = [n; s; w; e; ne; nw; se; sw]
end

let round (map, checklist) =
  let check pos dirs =
    List.for_all (fun d -> not @@ S.mem (add pos d) map) dirs
  in
  let propose checklist pos =
    if check pos D.directions then
      pos, pos
    else
      List.find_map (fun (dir, checks) ->
          if check pos checks then Some (pos, add pos dir) else None)
        checklist
      |> Option.value ~default: (pos, pos)
  in
  let move tbl (pos, dest) =
    if (List.length @@ Hashtbl.find_all tbl dest) > 1
    then pos
    else dest
  in
  let rotate = function
    | [] -> []
    | x::xs -> xs@[x]
  in
  let prs = map |> S.to_seq |> Seq.map (propose checklist) in
  let dests =
    let t = Hashtbl.create 100 in
    Seq.iter (fun (p, dest) -> Hashtbl.add t dest p) prs;
    t
  in
  Seq.map (move dests) prs |> S.of_seq, rotate checklist

let rec repeat f n x =
  if n = 0 then x
  else repeat f (n - 1) @@ f x

let rec stable i (map, checklist) =
  let nmap, checklist = round (map, checklist) in
  if S.equal map nmap then i+1
  else stable (i+1) (nmap, checklist)

let checklist =
  let open D in
  [ n, [n; ne; nw];
    s, [s; se; sw];
    w, [w; nw; sw];
    e, [e; ne; se];
  ]

let count_empty_land map =
  let map = S.to_seq map in
  let xmin = Seq.fold_left min max_int @@ Seq.map fst map
  and xmax = Seq.fold_left max min_int @@ Seq.map fst map
  and ymin = Seq.fold_left min max_int @@ Seq.map snd map
  and ymax = Seq.fold_left max min_int @@ Seq.map snd map in
  (xmax - xmin + 1) * (ymax - ymin + 1) - Seq.length map

let () = Printexc.record_backtrace true
let () =
  let data = open_in Sys.argv.(1) |> IO.input_lines |> parse in
  repeat round 10 (data, checklist)
  |> fst
  |> count_empty_land
  |> print_int;
  print_newline ();
  stable 0 (data, checklist)
  |> print_int
