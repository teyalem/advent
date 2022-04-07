open Ut

let parse ss =
  match ss with
  | ns :: boards ->
    let ns =
      String.split_on_char ',' ns
      |> List.map int_of_string
    in
    let parse_board b =
      let scan = Scanf.sscanf b " %d" in
      Mat.init 5 5 (fun _ _ -> scan Fun.id)
    in
    ns, List.map parse_board boards
  | _ -> assert false

let is_bingo marks =
  let f b = Array.(exists (for_all Fun.id) b) in
  f marks || f @@ Mat.transpose marks

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
  let marks = Mat.make 5 5 false in
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
          b
          |> Mat.mapi (fun x y e -> if not marks.(x).(y) then e else 0)
          |> Mat.fold Int.add 0
          |> Int.mul n
        in
        i, score
      else
        aux (i+1) ns
  in
  aux 1 ns

let () =
  let ns, boards = IO.read_all () |> Delim.split "\n\n" |> parse in
  let res = List.map (bingo ns) boards in
  let solve f i =
    res |> List.fold_left
      (fun (pi, ps) (ni, ns) -> if f ni pi then ni, ns else pi, ps)
      (i, -10)
    |> snd
    |> Printf.printf "%d\n"
  in
  begin
    (* PART 1 *) solve (<) Int.max_int;
    (* PART 2 *) solve (>) Int.min_int;
  end
