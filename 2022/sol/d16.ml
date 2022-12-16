open Ut

let parse_list str =
  String.split_on_char ',' str
  |> List.map String.trim

let parse str =
  Scanf.sscanf str "Valve %s has flow rate=%d; tunnel%c lead%c to valve%c %s@!"
    (fun v f _ _ _ nei -> v, (f, parse_list nei))

let prune graph =
  let valves =
    List.filter_map (fun (v, (f, _)) ->
        if f > 0 then Some (v, f) else None)
      graph
  in

  let bfs s e =
    let q = Queue.create () in
    let visited = Hashtbl.create 500 in
    let rec aux () =
      if Queue.is_empty q then assert false
      else
        let here, step = Queue.pop q in
        if Hashtbl.mem visited here then aux ()
        else if here = e then step
        else begin
          Hashtbl.add visited here true;
          graph
          |> List.assoc here
          |> snd
          |> List.iter (fun n -> Queue.add (n, step+1) q);
          aux ()
        end
    in
    Queue.add (s, 0) q;
    aux ()
  in

  (("AA", 0) :: valves)
  |> List.map (fun (s, f) ->
      s, (f, valves
             |> List.filter_map (fun (e, f2) ->
                 if s = e then None else Some (e, (bfs s e, f2)))))

let print graph =
  graph
  |> List.iter (fun (s, (f, neigh)) ->
      Printf.printf "%s(%d) : [" s f;
      List.iter (fun (e, (w, _)) -> Printf.printf " %s(%d)" e w) neigh;
      Printf.printf "]\n")

let all_pathes ?(sub = true) ?(pre = []) graph time_left =
  let rec aux (min, here, opened) : (string * int) list list =
    let _, neigh = List.assoc here graph in
    let over = ref false in
    let nexts =
      neigh
      |> List.filter (fun (dest, _) -> not @@ List.mem_assoc dest opened)
      |> List.map (fun (dest, (w, _)) ->
          let m = min + w + 1 in
          (m, dest, (dest, m)::opened))
      |> List.concat_map (fun (m, _, _ as state) ->
          if m >= time_left then
            begin over := true; [] end
          else aux state)
    in
    if sub || !over then opened :: nexts else nexts
  in
  aux (0, "AA", pre)

let calc_pressure graph time xs =
  xs
  |> List.map (fun (dest, m) ->
      let f, _ = List.assoc dest graph in
      f * (time - m))
  |> List.fold_left (+) 0

let find_maxp graph =
  all_pathes graph 30
  |> List.map (calc_pressure graph 30)
  |> List.fold_left max min_int

let find_maxp_part2 graph =
  let time_left = 26 in

  (* NOTE: following code is SO SLOW... (over 3min) *)
  all_pathes graph time_left
  |> List.to_seq
  |> Seq.concat_map (fun xs ->
      all_pathes ~sub: false ~pre: xs graph time_left
      |> List.to_seq)
  |> Seq.map (calc_pressure graph time_left)
  |> Seq.fold_left max min_int

let () =
  let data = open_in Sys.argv.(1) |> IO.input_lines |> List.map parse in
  let graph = prune data in
  find_maxp graph |> print_int;
  print_newline ();
  find_maxp_part2 graph |> print_int;
