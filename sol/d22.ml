open Advent

(* Combat *)
module Game = struct

  type 'a queue = 'a Queue.t
  type deck = int queue

  (* take n elements from queue *)
  (* fast version with Seq *)
  let take_n n queue =
    let rec limit_seq n seq () =
      if n = 0
      then Seq.Nil
      else match seq () with
        | Seq.Nil -> Seq.Nil
        | Seq.Cons (a, f) -> Seq.Cons (a, limit_seq (n-1) f)
    in
    limit_seq n @@ Queue.to_seq queue
    |> Queue.of_seq

  (* calculate score of given deck. *)
  let calculate_score deck =
    let open List in
    Queue.to_seq deck
    |> of_seq
    |> rev
    |> mapi (fun i n -> (i+1)*n)
    |> sum

  (* run a game of combat *)
  let rec run (p1, p2) =
    if Queue.is_empty p2 then calculate_score p1
    else if Queue.is_empty p1 then calculate_score p2
    else
      let c1 = Queue.take p1
      and c2 = Queue.take p2
      in begin (* assume every card is unique (no same number) *)
        if c1 > c2 (* p1 wins *)
        then begin
          Queue.add c1 p1;
          Queue.add c2 p1
        end
        else begin
          Queue.add c2 p2;
          Queue.add c1 p2
        end;

        run (p1, p2)
      end

  type winner = P1 | P2

  (* run a game of recursive combat *)
  let run_recur (p1, p2) =

    let update_deck winner (c1, p1) (c2, p2) =
      begin match winner with
        | P1 -> begin
            Queue.add c1 p1;
            Queue.add c2 p1
          end
        | P2 -> begin
            Queue.add c2 p2;
            Queue.add c1 p2
          end
      end;
      p1, p2
    in

    let rec run_rounds prev_rounds (p1, p2) =
      if Queue.is_empty p2 then P1, p1
      else if Queue.is_empty p1 then P2, p2

      else begin
        if List.exists (fun (d1, d2) -> d1 = p1 || d2 = p2) prev_rounds
        then P1, p1
        else begin
          (* Save copy of decks *)
          let prev_rounds = Queue.(copy p1, copy p2) :: prev_rounds in

          (* draw cards *)
          let c1 = Queue.take p1
          and c2 = Queue.take p2 in

          (* find winner *)
          let winner =
            if Queue.(c1 <= length p1 && c2 <= length p2)
            then
              let p1 = take_n c1 p1
              and p2 = take_n c2 p2
              in fst @@ run_rounds [] (p1, p2)

            else if c1 > c2 then P1 else P2
          in

          (* run next round *)
          run_rounds prev_rounds @@ update_deck winner (c1, p1) (c2, p2)
        end
      end
    in

    calculate_score @@ snd @@ run_rounds [] (p1, p2) (* run Game 1 *)

    (* Parse a deck *)
  let parse_deck str =
    Delim.split_line str
    |> List.tl
    |> List.map int_of_string
    |> List.to_seq
    |> Queue.of_seq

  let parse_decks str =
    match Delim.split "\n\n" str with
    | [p1; p2] ->
      parse_deck p1, parse_deck p2
    | _ -> assert false

end

let main path =
  let p1, p2 = open_in path |> IO.read_file |> Game.parse_decks in
  begin
    (* PART 1 *)
    let d1, d2 = Queue.(copy p1, copy p2) in
    Game.run (d1, d2) |> print_int;

    print_newline ();

    (* PART 2 *)
    let d1, d2 = Queue.(copy p1, copy p2) in
    Game.run_recur (d1, d2) |> print_int;
  end

let _ = Arg.parse [] main ""
