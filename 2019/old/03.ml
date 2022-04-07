type dir =
    | Up
    | Down
    | Left
    | Right
type point = int * int
type dis = dir * int (*displacement*)
type line = point * dis (*a line*)

exception NoMatchingException

let format_dir = function
    |Up -> "Up"
    |Down -> "Down"
    |Left -> "Left"
    |Right -> "Right"

let print_dir d =
    print_string (format_dir d)

(*parsers*)
let parse_dir = function
    |'U' -> Up
    |'D' -> Down
    |'L' -> Left
    |'R' -> Right
    |_ -> raise NoMatchingException

let parse_d (x: string) : dis =
    let d = parse_dir x.[0] in
    let n = int_of_string (String.sub x 1 ((String.length x) - 1)) in
    (d, n)

let parse_dl s =
    let rec parse_dl_rec ds = function
        |s::ss -> parse_dl_rec ((parse_d s)::ds) ss
        |[] -> ds
    and ss = String.split_on_char ',' s in
    parse_dl_rec [] ss

(*actions*)
let move_point (x, y) = function
    |(Up, d) -> (x, y+d)
    |(Down, d) -> (x, y-d)
    |(Right, d) -> (x+d, y)
    |(Left, d) -> (x-d, y)

let displace (p, od) d =
    let np = move_point p od in
    (np, d)

(*test functions*)
let is_cross a b =
    let x (n, _) = n in
    let y (_, n) = n in
    let in_range n (a, b) = a <= n and n <= b in
    let (p, (d, n)) = a in
    let t = match d with
    |

let solver ds1 ds2 =
    let rec solver_tail ds1 
    solver_tail 

let file = "input03"

let _ =
    let chan = open_in file in
    let ds1 = parse_dl (input_line chan) in
    let ds2 = parse_dl (input_line chan) in
