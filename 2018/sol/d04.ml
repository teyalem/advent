open Ut

let parse str =
  Scanf.sscanf str "[1518-%d-%d %d:%d] %s@!"
    (fun mon d h min msg -> (mon, d), (h, min), msg)

let (|?) a b =
  if a = 0 then b else a

module P = struct
  type t = int * int

  let compare (a, b) (c, d) =
    Int.compare a c |? Int.compare b d
end

module M = struct
  module G = Map.Make(Int)
  module D = Map.Make(P)

  let empty = G.empty
  let add m (g, d) e =
    m |> G.update g (function
        | None -> Some (D.singleton d [e])
        | Some dm ->
          D.update d (function None -> Some [e] | Some xs -> Some (e::xs)) dm
          |> Option.some)
end

let collect tt =
  tt
  |> List.sort (fun (da, ha, _) (db, hb, _) -> P.compare da db |? P.compare ha hb)
  |> List.fold_left (fun (cg, m) (date, (_, min), msg) ->
      if String.starts_with ~prefix: "Guard" msg then
        let g = Scanf.sscanf msg "Guard #%d begins shift" Fun.id in
        g, m
      else if msg = "wakes up" then
        cg, M.add m (cg, date) (min, 'w')
      else if msg = "falls asleep" then
        cg, M.add m (cg, date) (min, 's')
      else assert false)
    (-1, M.empty)
  |> snd

let timetable xs =
  let arr = Array.make 60 0 in
  let rec aux = function
    | [] -> ()
    | (m, 's') :: (n, 'w') :: xs ->
      for i = m to n-1 do arr.(i) <- 1 done;
      aux xs
    | x ->
      List.iter (fun (m, c) -> Printf.printf "[%d %c]\n" m c) x;
      assert false
  in
  xs
  |> List.sort (fun (ma, _) (mb, _) -> Int.compare ma mb)
  |> aux;
  arr

let sleeptime ds =
  M.D.fold (fun _ n acc -> acc + n)
    (M.D.map (Array.fold_left Int.add 0) ds)
    0

let sleep_chart ds =
  M.D.fold (fun _ t acc -> Array.map2 Int.add t acc) ds (Array.make 60 0)

let most_likely_sleep ds =
  sleep_chart ds
  |> Array.to_seqi
  |> Seq.fold_left
    (fun (_, pp as p) (_, np as n) -> if np > pp then n else p)
    (0, -1)
  |> fst

let strat1 gm =
  M.G.to_seq gm
  |> Seq.fold_left (fun (p, pt) (_, v as n) ->
      let nt = sleeptime v in
      if nt > pt then n, nt else p, pt)
    ((0, M.D.empty), -1)
  |> fst
  |> (fun (k, ds) -> k * most_likely_sleep ds)

let strat2 gm =
  (M.G.map sleep_chart gm)
  |> M.G.to_seq
  |> Seq.map (fun (k, v) -> Array.map (fun n -> k, n) v)
  |> Seq.fold_left
    (Array.map2 (fun (_, na as a) (_, nb as b) -> if na > nb then a else b))
    (Array.make 60 (-1, -1))
  |> Array.mapi (fun i (k, n) -> k, i, n)
  |> Array.fold_left
    (fun (_, _, na as a) (_, _, nb as b) -> if na > nb then a else b)
    (0, 0, -1)
  |> (fun (g, n, _) -> g * n)

let () =
  let data =
    IO.read_lines ()
    |> List.sort String.compare
    |> List.map parse
    |> collect
    |> M.G.map (M.D.map timetable)
  in
  (* PART 1 *) data |> strat1 |> print_int;
  print_newline ();
  (* PART 2 *) data |> strat2 |> print_int;
