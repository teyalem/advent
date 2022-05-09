open Ut

let perpendicular (a, b) (c, d) =
  a*c + b*d = 0

let add (a, b) (c, d) =
  a+c, b+d

let follow_line map start =
  let dx = Mat.dimx map and dy = Mat.dimy map in
  let rec aux step letters (x, y as pos) delta =
    match (try map.(x).(y) with _ -> ' ') with
    | ' ' -> step, letters (* out-of-line; end *)
    | '+' ->
      let delta =
        Neigh.von_neumann
        |> List.find (fun d ->
            let x, y = add pos d in
            0 <= x && x <= dx && 0 <= y && y <= dy
            && map.(x).(y) <> ' '
            && perpendicular delta d)
      in
      aux (step+1) letters (add pos delta) delta
    | c ->
      let letters = if 'A' <= c && c <= 'Z' then c :: letters else letters in
      aux (step+1) letters (add pos delta) delta
  in
  let step, letters = aux 0 [] start (0, 1) in
  step, List.rev letters

let find_start map =
  Mat.get_row map 0
  |> Array.to_seq
  |> Seq.mapi (fun i x -> i, x)
  |> Seq.find_map (fun (i, x) -> if x = '|' then Some (i, 0) else None)

let () =
  let data =
    IO.read_lines ()
    |> List.to_seq
    |> Seq.map String.to_seq
    |> Mat.of_seq
    |> Mat.transpose
  in
  (* PART 1 & 2 *)
  find_start data
  |> Option.map @@ follow_line data
  |> Option.iter @@ fun (step, letters) ->
  List.iter (Printf.printf "%c") letters;
  Printf.printf "\n%d\n" step
