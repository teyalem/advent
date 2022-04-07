open Ut

let move_finger inpad (x, y) d =
  let nx, ny =
    match d with
    | 'U' -> x, y-1
    | 'D' -> x, y+1
    | 'L' -> x-1, y
    | 'R' -> x+1, y
    | _ -> assert false
  in
  if inpad nx ny then nx, ny else x, y

let inpad1 x y = 0 <= x && x <= 2 && 0 <= y && y <= 2

let inpad2 x y = abs x + abs y <= 2

let next_finger inpad cur = Seq.fold_left (move_finger inpad) cur

let to_digit1 (x, y) = 3*y + x + 1 |> string_of_int

let to_digit2 (x, y) =
  let arr = [|
    [|  "";  ""; "1";  "";  "" |]; 
    [|  ""; "2"; "3"; "4";  "" |]; 
    [| "5"; "6"; "7"; "8"; "9" |]; 
    [|  ""; "A"; "B"; "C";  "" |]; 
    [|  "";  ""; "D";  "";  "" |];
  |]
  in
  arr.(y+2).(x+2)

let find_code start inpad to_num insts =
  insts
  |> Seq.scan (next_finger inpad) start
  |> Seq.drop 1
  |> Seq.map to_num
  |> List.of_seq
  |> String.concat ""

let () =
  let data = IO.read_lines () |> List.map String.to_seq |> List.to_seq in
  begin
    (* PART 1 *)
    find_code (1, 1) inpad1 to_digit1 data |> Printf.printf "%s\n";

    (* PART 2 *)
    find_code (-2, 0) inpad2 to_digit2 data |> Printf.printf "%s\n";
  end
