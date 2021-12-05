open Ut

let parse str =
  let ic = Scanf.Scanning.from_string str in
  let scan = Scanf.bscanf in
  let ns =
    scan ic "%s@\n" (fun s ->
        String.split_on_char ',' s
        |> List.map int_of_string)
  in
  let rec pboard () =
    let b = Array.make_matrix 5 5 0 in
    try
      for y = 0 to 4 do
        for x = 0 to 4 do
          scan ic " %d" (fun n -> b.(x).(y) <- n)
        done
      done;
      b :: pboard ()
    with End_of_file -> []
  in
  ns, pboard ()

let is_bingo marks =
  let open Array in
  let is_bingo_col =
    exists (for_all Fun.id) marks
  and is_bingo_row =
    init 5 (fun i -> map (fun l -> l.(i)) marks)
    |> exists (for_all Fun.id)
  in
  is_bingo_row || is_bingo_col

let numloc n b : int * int =
  let pos = ref (0, 0) in
  try
    for x = 0 to 4 do
      for y = 0 to 4 do
        if b.(x).(y) = n then
          begin pos := x, y; raise Exit end
      done
    done;
    -1, -1
  with Exit -> !pos

let bingo ns b : int * int =
  let marks = Array.make_matrix 5 5 false in
  let mark n =
    let x, y = numloc n b in
    if x <> -1 then marks.(x).(y) <- true
  in
  let rec aux i = function
    | [] -> Int.max_int, 0
    | n::ns ->
      mark n;
      if is_bingo marks then
        let score =
          Array.mapi
            (fun x l ->
               Array.to_list l
               |> List.filteri (fun y _ -> not marks.(x).(y))
               |> List.fold_left Int.add 0)
            b
          |> Array.fold_left Int.add 0
          |> Int.mul n
        in
        i, score
      else
        aux (i+1) ns
  in
  aux 1 ns

let () =
  let ns, boards = open_in Sys.argv.(1) |> IO.input_all |> parse in
  begin
    let res = List.map (bingo ns) boards in

    (* PART 1 *)
    res
    |> List.fold_left (fun (pi, ps) (ni, ns) ->
        if ni < pi then ni, ns else pi, ps)
      (Int.max_int, -10)
    |> snd
    |> Printf.printf "%d\n";

    (* PART 2 *)
    res
    |> List.fold_left (fun (pi, ps) (ni, ns) ->
        if ni > pi then ni, ns else pi, ps)
      (Int.min_int, -10)
    |> snd
    |> Printf.printf "%d\n"
  end
