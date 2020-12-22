open Advent

(* Combat *)
module Game = struct

  type deck = int list

  (* calculate score of given deck. *)
  let calculate_score deck =
    let open List in
    rev deck
    |> mapi (fun i n -> (i+1)*n)
    |> sum

  (* run a game of combat *)
  let rec run = function
    | [], [] -> raise (Invalid_argument "run")
    | p, [] | [], p -> calculate_score p (* winning state *)

    | c1::p1, c2::p2 ->
      run @@
      (if c1 > c2 (* if p1 wins, assume every card is unique (no same number) *)
       then p1 @ [c1; c2], p2
       else p1, p2 @ [c2; c1])

  type winner = P1 | P2

  (* run a game of recursive combat *)
  (* TOO SLOW *)
  let run_recur (p1, p2) =
      let open List in

    let update_deck winner p1 p2 =
      let c1 = hd p1 and p1 = tl p1
      and c2 = hd p2 and p2 = tl p2 in
      match winner with
      | P1 -> p1 @ [c1; c2], p2
      | P2 -> p1, p2 @ [c2; c1]
    in

    let rec run_rounds prev_rounds = function
      | [], [] -> assert false
      | p1, [] -> P1, p1
      | [], p2 -> P2, p2
      | p1, p2 ->
        let winner = round_winner prev_rounds p1 p2 in
        if mem (p1, p2) prev_rounds
        then P1, p1
        else
          let prev_rounds = (p1, p2) :: prev_rounds in
          run_rounds prev_rounds @@ update_deck winner p1 p2

    and round_winner prev_rounds p1 p2 =
      if mem (p1, p2) prev_rounds (* loop detected *)
      then P1
      else 
        match p1, p2 with

        | c1::p1, c2::p2 ->
          if c1 <= length p1 && c2 <= length p2
          then (* run subgame *)
            let p1 = take c1 p1
            and p2 = take c2 p2
            in fst @@ run_rounds [] (p1, p2)
          else (* default *)
          if c1 > c2 then P1 else P2

        | _ -> assert false
    in

    calculate_score @@ snd @@ run_rounds [] (p1, p2) (* run Game 1 *)

    (* Parse a deck *)
  let parse_deck str =
    Delim.split_line str
    |> List.tl
    |> List.map int_of_string

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
    Game.run (p1, p2) |> print_int;

    print_newline ();

    (* PART 2 *)
    Game.run_recur (p1, p2) |> print_int;
  end

let _ = Arg.parse [] main ""
