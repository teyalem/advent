open Ut

module Dice = struct
  type t = int * int

  let dice = 1, 0

  let roll (d, i) : int * t =
    if d = 100 then 100, (1, i+1)
    else d, (d+1, i+1)

  let roll3 d =
    let a, d = roll d in
    let b, d = roll d in
    let c, d = roll d in
    a+b+c, d

  let count (_, i) = i
end

module Player = struct
  type t = { pos : int; score : int; }

  let make pos = { pos; score = 0; }

  let parse str =
    Scanf.sscanf str "Player %d starting position: %d"
      (fun _ n -> make n)

  let move p n =
    let pos =
      let n = (p.pos + n) mod 10 in
      if n = 0 then 10 else n
    in
    { pos; score = p.score + pos }
end

let parse ss =
  match List.map Player.parse ss with
  | p1 :: p2 :: [] -> p1, p2
  | _ -> assert false

let turn_part1 d (p1, p2 : Player.t * Player.t) =
  let n, d = Dice.roll3 d in
  let p1 = Player.move p1 n in
  if p1.score >= 1000 then
    Either.Right (p2.score, Dice.count d)
  else
    let n, d = Dice.roll3 d in
    let p2 = Player.move p2 n in
    if p2.score >= 1000 then
      Right (p1.score, Dice.count d)
    else
      Left (d, p1, p2)

let solve_part1 ps =
  let rec aux d ps =
    match turn_part1 d ps with
    | Left (d, p1, p2) -> aux d (p1, p2)
    | Right (score, rolld) -> score * rolld
  in
  aux Dice.dice ps

let roll3 =
  let open List in
  init 3 succ |> concat_map (fun i ->
      init 3 succ |> concat_map (fun j ->
          init 3 succ |> map (fun k -> i+j+k)))
  |> sort Int.compare
  |> group_count

let solve_part2 (p1, p2) =
  let move p n =
    let p = Player.move p n in
    if p.score >= 21 then None else Some p
  in

  (* memoization for the win *)
  let memo_w = Hashtbl.create 10
  and memo_b = Hashtbl.create 10 in

  let rec white p1 p2 : int * int =
    if Hashtbl.mem memo_w (p1, p2) then
      Hashtbl.find memo_w (p1, p2)
    else
      roll3 |> List.fold_left (fun (w1, w2) (n, i) ->
          match move p1 n with
          | None -> w1+i, w2
          | Some p1 ->
            let iw1, iw2 = black p1 p2 in
            w1 + i*iw1, w2 + i*iw2)
        (0, 0)
      |> (fun w -> Hashtbl.add memo_w (p1, p2) w; w)

  and black p1 p2 : int * int =
    if Hashtbl.mem memo_b (p1, p2) then
      Hashtbl.find memo_b (p1, p2)
    else
      roll3 |> List.fold_left (fun (w1, w2) (n, i) ->
          match move p2 n with
          | None -> w1, w2+i
          | Some p2 ->
            let iw1, iw2 = white p1 p2 in
            w1 + i*iw1, w2 + i*iw2)
        (0, 0)
      |> (fun w -> Hashtbl.add memo_b (p1, p2) w; w)
  in
  white p1 p2 |> (fun (a, b) -> max a b)

let () =
  let data = IO.read_lines () |> parse in
  begin
    (* PART 1 *)
    solve_part1 data |> Printf.printf "%d\n";

    (* PART 2 *)
    solve_part2 data |> Printf.printf "%d\n";
  end
