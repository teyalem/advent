open Ut

module Tile = struct
  type t = Empty | Wall | Block | Paddle | Ball

  let default = Empty

  let tiles = [| Empty; Wall; Block; Paddle; Ball |]

  let of_int i = tiles.(i)
  let to_int = function
    | Empty -> 0
    | Wall -> 1
    | Block -> 2
    | Paddle -> 3
    | Ball -> 4

  let of_char _ = assert false
  let to_char = function
    | Empty -> '.'
    | Wall -> '|'
    | Block -> '#'
    | Paddle -> '-'
    | Ball -> 'o'
end

module Screen = Block.Make(Tile)

module Game = struct
  type t = { mutable score: int;
             m: IntCode.t;
             scr: Screen.t; }

  let load c = { score = 0;
                 m = IntCode.load c;
                 scr = Screen.make 44 21; }

  let set_free g = IntCode.set g.m 0 2
  let print_screen g =
    Screen.print g.scr

  let update_screen g =
    let rec loop () =
      let open IntCode in
      let x = get_output g.m in
      let y = get_output g.m in
      let c = get_output g.m in

      if x = -1 && y = 0
      then begin g.score <- c; Printf.printf "score: %d\n" c end
      else Screen.set g.scr x y @@ Tile.of_int c;

      if is_output_empty g.m
      then ()
      else loop ()
    in
    loop ()

  let test_run g =
    IntCode.run g.m;
    update_screen g;
    Screen.count_occur Tile.Block g.scr

  let run g =
    let rec loop () =
      let open IntCode in
      Screen.print g.scr;
      let p =
        match read_line () with
        | "a" -> -1
        | "s" -> 0
        | "d" -> 1
        | _ -> 0
      in
      set_input g.m p;
      run g.m;
      update_screen g;

      if is_halt g.m || Screen.count_occur Tile.Block g.scr = 0
      then ()
      else loop ()
    in
    loop ()

  exception Return of int
  let hack_rom g =
    let open IntCode in
    let len = code_length g.m in
    let paddle =
      match
        for i = 0 to len - 4 do
          if get g.m i = 0
          && get g.m (i+1) = 3
          && get g.m (i+2) = 0
          then raise (Return i)
          else ()
        done
      with () -> -1 | exception Return i -> i + 3
    in
    try
      for i = paddle to len do
        if get g.m i = 1 && get g.m (i+1) > 4
        then raise Exit
        else set g.m i 1
      done
    with _ -> ()

  let count_block_scores g =
    let open IntCode in
    let len = code_length g.m in
    let score = ref 0 in
    let score_table =
      let s = ref 0 in 
      match for i = 0 to len - 1 do
          if get g.m i = 1 && get g.m (i+1) > 4
          then begin s := i+1; raise Exit end
          else ()
        done
      with () -> 0
         | exception Exit -> !s
    in
    let dimx = Screen.dimx g.scr in
    Screen.iteri (fun x y c ->
        if c = Tile.Block
        then score := !score + (get g.m (score_table + y*dimx + x))
        else ()) g.scr;
    !score

end

let main path =
  let data = open_in path |> IO.read_file |> IntCode.parse_code in
  begin
    (* PART 1 *)
    let g = Game.load data in
    Game.test_run g |> print_int;

    print_newline ();

    (* PART 2 *)
    let g = Game.load data in
    Game.set_free g;
    Game.hack_rom g;
    Game.count_block_scores g |> print_int;
    print_newline ();

    Game.run g;
    Game.print_screen g;
    print_int g.score;
  end

let _ = Arg.parse [] main ""
