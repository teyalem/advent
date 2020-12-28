open Advent

module Color = struct
  type t = Black | White

  let default = Black

  let to_int = function
    | Black -> 0
    | White -> 1

  let of_int = function
    | 0 -> Black
    | 1 -> White
    | _ -> assert false

  let of_char = function
    | '.' -> Black
    | '#' -> White
    | _ -> assert false

  let to_char = function
    | Black -> '.'
    | White -> '#'

end

module Direction = struct
  type t = Up | Down | Left | Right

  let clockwise = [| Up; Right; Down; Left |]

  let to_int d =
    match d with
    | Up -> 0
    | Right -> 1
    | Down -> 2
    | Left -> 3

  let of_int d = clockwise.((d+4) mod 4)

  let turn_left d =
    to_int d - 1 |> of_int

  let turn_right d =
    to_int d + 1 |> of_int

  let to_pos d =
    match d with
    | Up -> 0, -1
    | Right -> 1, 0
    | Down -> 0, 1
    | Left -> -1, 0

end

module Hull = Block.Make(Color)

module Bot = struct
  type t = { mutable pos: int * int;
             mutable dir: Direction.t; }

  let init () = { pos = 0, 0;
                  dir = Direction.Up; }

  let count_painted bot code =
    let m = IntCode.load code in
    let tiles = Hashtbl.create 2000 in
    let rec loop () =
      let x, y = bot.pos in
      let c = Hashtbl.find_opt tiles (x, y)
              |> Option.value ~default: Color.Black
      in
      IntCode.set_input m @@ Color.to_int c;
      IntCode.run m;

      let color = Color.of_int @@ IntCode.get_output m in
      Hashtbl.replace tiles (x, y) color;

      let turn = IntCode.get_output m in
      bot.dir <-
        if turn = 0
        then Direction.turn_left bot.dir
        else Direction.turn_right bot.dir;

      let dx, dy = Direction.to_pos bot.dir in
      bot.pos <- x + dx, y + dy;

      if IntCode.is_halt m
      then Hashtbl.length tiles
      else loop ()
    in
    loop ()


  let run bot code hull =
    let m = IntCode.load code in
    let rec loop () =
      let x, y = bot.pos in
      Hull.get hull x y
      |> Color.to_int 
      |> IntCode.set_input m;
      IntCode.run m;

      let color = Color.of_int @@ IntCode.get_output m in
      Hull.set hull x y color;

      let turn = IntCode.get_output m in
      bot.dir <-
        if turn = 0
        then Direction.turn_left bot.dir
        else Direction.turn_right bot.dir;

      let dx, dy = Direction.to_pos bot.dir in
      bot.pos <- x + dx, y + dy;

      if IntCode.is_halt m
      then ()
      else loop ()
    in
    loop ()

end

let main path =
  let data = open_in path |> IO.read_file |> IntCode.parse_code in
  begin
    (* PART 1 *)
    let n = Bot.count_painted (Bot.init ()) data in
    print_int n;

    print_newline ();
    (* PART 2 *)
    let hull = Hull.make 50 8 in
    Hull.(set hull 0 0 Color.White);
    Bot.run (Bot.init ()) data hull;
    Hull.print hull;

  end

let _ = Arg.parse [] main ""
