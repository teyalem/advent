open Ut

module S = Set.Make(Char)
module M = Map.Make(Char)

let collect =
  let add m (k, v) =
    m
    |> M.update k (function None -> Some (S.singleton v) | Some s -> Some (S.add v s))
    |> M.update v (function None -> Some S.empty | s -> s)
  in
  List.fold_left add M.empty

let finish (m : S.t M.t) (e : char) =
  M.map (S.remove e) m
  |> M.partition (fun _ s -> S.is_empty s)
  |> (fun (fs, xs) -> M.to_seq fs |> Seq.map fst |> S.of_seq, xs)

let of_graph m =
  let q, m = finish m '_' in
  q, m

let is_job_avail (q, _) = not @@ S.is_empty q

let pop (q, m) =
  let e = S.min_elt q in
  e, (S.remove e q, m)

let finish_job j (q, m) =
  let fin, m = finish m j in
  S.union q fin, m

let complete_order m =
  of_graph m
  |> Seq.unfold (fun q ->
      if is_job_avail q then
        let e, q = pop q in
        let q = finish_job e q in
        Some (e, q)
      else None)
  |> String.of_seq

let worktime c = 60 + Char.(code c - code 'A' + 1)

let add_worker t j ws =
  (t + worktime j, j) :: ws

let end_worker t ws =
  List.partition (fun (k, _) -> k = t) ws
  |> (fun (a, b) -> List.hd a |> snd, b)

let min_endtime ws =
  List.map fst ws |> List.fold_left min max_int

let multiworker m =
  let rec dist t ws q =
    if List.length ws >= 5 || not @@ is_job_avail q
    then work t ws q
    else let j, q = pop q in dist t (add_worker t j ws) q

  and work _ ws q =
    let t = min_endtime ws in
    let j, ws = end_worker t ws in
    let q = finish_job j q in
    if ws = [] && not @@ is_job_avail q then t
    else if List.length ws < 5 && is_job_avail q
    then dist t ws q
    else work t ws q
  in
  dist 0 [] (of_graph m)

let parse str =
  Scanf.sscanf str "Step %c must be finished before step %c can begin."
    (fun a b -> b, a)

let () =
  let data = IO.read_lines () |> List.map parse |> collect in
  (* PART 1 *) complete_order data |> print_endline;
  (* PART 2 *) multiworker data |> print_int;
