module Game = struct
  (* turn start at 1 *)
  type t = {
    mutable last_number: int;
    mutable last_turn: int;
    numbers: int array;
  }

  let make start_nums max_turn =
    let len = List.length start_nums in

    (* make and initialize numbers array *)
    let arr = Array.make max_turn 0 in
    for i = 0 to len - 2 do
      let n = List.nth start_nums i in
      arr.(n) <- i+1
    done;

    { last_number = List.nth start_nums (len - 1);
      last_turn  = len;
      numbers = arr; }

  let find_turn game n =
    match game.numbers.(n) with
    | 0 -> None
    | i -> Some i

  let set_assoc game n turn = game.numbers.(n) <- turn

  let run game limit =
    for i = game.last_turn + 1 to limit do
      let said_turn = find_turn game game.last_number in
      let delta = match said_turn with
        | None -> 0
        | Some turn -> game.last_turn - turn
      in

      (* push last number *)
      set_assoc game game.last_number game.last_turn;

      game.last_number <- delta;
      game.last_turn <- i; (* end turn *)
    done

  let get_last game = game.last_number

end

let () =
  let input = [19; 20; 14; 0; 9; 1] in
  begin
    (* PART 1 *)
    let max_turn = 2020 in
    let game = Game.make input max_turn in
    Game.run game max_turn;
    Game.get_last game |> print_int;

    print_newline ();

    (* PART 2 *)
    let max_turn = 30_000_000 in
    let game = Game.make input max_turn in
    Game.run game max_turn;
    Game.get_last game |> print_int;
  end
