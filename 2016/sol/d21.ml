open Ut

type inst =
  | Swap_position of int * int
  | Swap_letter of char * char
  | Rotate of bool * int
  | Rotate_on of char
  | Reverse of int * int
  | Move of int * int

let parse str =
  let int = int_of_string in
  match String.split_on_char ' ' str with
  | ["swap"; "position"; x; _; _; y] -> Swap_position (int x, int y)
  | ["swap"; "letter"; x; _; _; y] -> Swap_letter (x.[0], y.[0])
  | ["rotate"; dir; x; ("step"|"steps")] -> Rotate (dir = "left", int x)
  | ["rotate"; "based"; "on"; _; _; _; x] -> Rotate_on x.[0]
  | ["reverse"; _; x; _; y] -> Reverse (int x, int y)
  | ["move"; _; x; _; _; y] -> Move (int x, int y)
  | _ -> assert false

let perform ?(rev=false) str inst =
  let open String in
  let len = length str in
  match inst with
  | Swap_position (xi, yi) ->
    init len (fun i -> if i = xi then str.[yi]
               else if i = yi then str.[xi]
               else str.[i])

  | Swap_letter (x, y) ->
    map (fun c -> if c = x then y else if c = y then x else c) str

  | Rotate (dir, x) ->
    let f =
      if (if rev then not dir else dir)
      then (* left *) fun i -> (i+x) mod len
      else (* right *) fun i -> (i-x+len) mod len
    in
    init len (fun i -> str.[f i])

  | Rotate_on x ->
    let xi = index str x in
    let sh =
      List.nth
        (if rev then [9; 1; 6; 2; 7; 3; 8; 4] else [1; 2; 3; 4; 6; 7; 8; 9])
        xi
    in
    init len (fun i -> str.[(i-sh+2*len) mod len])

  | Reverse (xi, yi) ->
    init len (fun i ->
        let i = if xi <= i && i <= yi then yi - i + xi else i in
        str.[i])

  | Move (xi, yi) ->
    let xi, yi = if rev then yi, xi else xi, yi in
    let x = str.[xi] in
    let t = init (len-1) (fun i -> str.[if i < xi then i else i+1]) in
    init len (fun i ->
        if i < yi then t.[i]
        else if i = yi then x
        else t.[i-1])

let pw = "abcdefgh"
let spw = "fbgdceah"

let () =
  let data = IO.read_lines () |> List.map parse in

  (* PART 1 *)
  List.fold_left perform pw data |> Printf.printf "%s\n";

  (* PART 2 *)
  List.rev data
  |> List.fold_left (perform ~rev: true) spw
  |> Printf.printf "%s\n"
