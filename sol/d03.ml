open Ut

let delta = function
  | '^' ->  0, -1
  | 'v' ->  0,  1
  | '>' ->  1,  0
  | '<' -> -1,  0
  | _ -> assert false

let nuniqs l =
  List.(sort_uniq Stdlib.compare l |> length)

let part1 seq =
  seq |> Seq.fold_left (fun ((x, y), l) d ->
      let dx, dy = delta d in
      let p = x + dx, y + dy in
      p, p::l)
    ((0, 0), [0, 0])
  |> snd
  |> nuniqs

let part2 seq =
  seq |> Seq.fold_left (fun ((x, y), (rx, ry), l, turn) d ->
      let dx, dy = delta d in
      if turn then
        let p = x+dx, y+dy in
        p, (rx, ry), p::l, false
      else
        let p = rx+dx, ry+dy in
        (x, y), p, p::l, true)
    ((0, 0), (0, 0), [0, 0], true)
  |> (fun (_, _, l, _) -> l)
  |> nuniqs

let () =
  let data = IO.read_all () |> String.to_seq in
  let f g = g data |> Printf.printf "%d\n" in
  begin
    (* PART 1 *) f part1;
    (* PART 2 *) f part2;
  end
