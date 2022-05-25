open Ut

module P = struct
  type t = int * int

  let compare (a, b) (c, d) =
    match Int.compare a c with
    | 0 -> Int.compare b d
    | v -> v
end

module S = Set.Make(P)

let pull set =
  S.to_seq set
  |> Seq.map (fun e -> e, S.remove e set)

let connect (ps, last, pairs) =
  let ms, rest = S.partition (fun (a, b) -> a = last || b = last) pairs in
  if S.is_empty ms
  then Seq.return (ps, last, S.empty) (* empty set to stop *)
  else
    pull ms
    |> Seq.map (fun ((a, b as e), r) ->
        e::ps, (if a = last then b else a), S.union r rest)

let gen_bridges pairs =
  let rec aux (_, _, pairs as tp) =
    if S.is_empty pairs
    then Seq.return tp
    else connect tp |> Seq.concat_map aux
  in
  aux ([], 0, pairs)
  |> Seq.map (fun (ps, _, _) -> ps)

let score ps =
  List.map (fun (a, b) -> a + b) ps
  |> List.fold_left Int.add 0

let parse str =
  Scanf.sscanf str "%d/%d" (fun a b -> a, b)

let () =
  let data = IO.read_lines () |> List.map parse |> S.of_list in
  let bridges = gen_bridges data |> Seq.memoize in
  (* PART 1 *)
  bridges
  |> Seq.map score
  |> Seq.fold_left max min_int
  |> print_int;
  print_newline ();

  (* PART 2 *)
  bridges
  |> Seq.map (fun ps -> List.length ps, score ps)
  |> Seq.fold_left (fun a b -> if P.compare a b > 0 then a else b) (0, min_int)
  |> snd
  |> print_int
