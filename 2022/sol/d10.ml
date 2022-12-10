open Ut

let inst_cycle = function
  | "addx" -> 2
  | "noop" -> 1
  | _ -> assert false

let step (cycle, x, c, (com, n), insts) =
  if c = -1 then None
  else if c = 0 then
    let nx =
      match com with
      | "addx" -> x + n
      | "noop" -> x
      | _ -> assert false
    in
    match insts with
    | [] -> Some ((cycle, nx), (cycle+1, nx, -1, (com, n), []))
    | (com, _ as inst) :: rest ->
      let nc = inst_cycle com in
      Some ((cycle, nx), (cycle+1, nx, nc-1, inst, rest))
  else
    Some ((cycle, x), (cycle+1, x, c-1, (com, n), insts))

let machine = function
  | [] -> assert false
  | (c, _ as head) :: rest ->
    Seq.unfold step (1, 1, inst_cycle c, head, rest)

let step2 (cycle, x, insts) =
  match insts with
  | [] -> None
  | (com, n) :: rest ->
    let nx =
      match com with
      | "addx" -> x + n
      | "noop" -> x
      | _ -> assert false
    in
    let cycle = cycle + inst_cycle com in
    Some ((cycle, nx), (cycle, nx, rest))

let streamline m =
  let rec aux cycle x m () =
    match Seq.uncons m with
    | None -> Seq.Nil
    | Some ((nc, nx), rest) ->
      if cycle = nc then
        Seq.Cons ((nc, nx), aux (nc+1) nx rest)
      else
        Seq.Cons ((cycle, x), aux (cycle+1) x (Seq.cons (nc, nx) rest))
  in
  aux 1 1 m

let machine2 insts =
  streamline @@ Seq.unfold step2 (1, 1, insts)

let parse str =
  if String.starts_with ~prefix: "noop" str then
    "noop", 0
  else
    Scanf.sscanf str "addx %d" (fun n -> "addx", n)

let draw m =
  let dp = Array.make_matrix 40 6 ' ' in
  m
  |> Seq.iter (fun (cycle, sx) ->
      let c = cycle - 1 in
      let x = c mod 40 and y = c / 40 in
      if sx - 1 <= x && x <= sx + 1 then
        dp.(x).(y) <- '#');
  for y = 0 to 5 do
    for x = 0 to 39 do
      print_char dp.(x).(y)
    done;
    print_newline ()
  done

let () =
  let data = open_in Sys.argv.(1) |> IO.input_lines |> List.map parse in
  let m = machine data in
  let m2 = machine2 data in
  assert (Seq.for_all2 (fun (c1, x1) (c2, x2) ->
      Printf.printf "[%d %d] [%d %d]\n" c1 x1 c2 x2;
      c1 = c2 && x1 = x2)
            m m2);
  let i = List.init 6 (fun n -> 20 + 40*n) in
  m |> Seq.filter (fun (c, _) -> List.mem c i)
  |> Seq.map (fun (c, x) -> c * x)
  |> Seq.fold_left (+) 0
  |> print_int;
  print_newline ();

  draw m
    (*
  |> Seq.take 100
  |> Seq.iter (fun (cycle, x) -> Printf.printf "%d %d\n" cycle x)
       *)
