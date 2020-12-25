open Advent

(* last row and column *)
let last_row = 127
let last_col = 7

(* row, column *)
type seat = int * int

type half = Lower | Upper

exception Invalid_seat_char

let half_of_row = function
  | 'F' -> Lower
  | 'B' -> Upper
  | _ -> raise Invalid_seat_char

let half_of_col = function
  | 'L' -> Lower
  | 'R' -> Upper
  | _ -> raise Invalid_seat_char

let rec bfind first last = function
  | [] -> if first = last then Some first else None
  | x::xs -> 
    let middle = first + (last - first)/2 in
    match x with
    | Lower -> bfind first middle xs
    | Upper -> bfind (middle+1) last xs

let decode str =
  let row_seq = String.(sub str 0 7 |> to_seq) |> Seq.map half_of_row
  and col_seq = String.(sub str 7 3 |> to_seq) |> Seq.map half_of_col
  in List.(of_seq row_seq, of_seq col_seq)

let to_seat (row_code, col_code) =
  let row_pos = bfind 0 last_row row_code
  and col_pos = bfind 0 last_col col_code
  in row_pos, col_pos

let to_sid (row, col) = row*8 + col

let main path =
  let open List in
  let file = open_in path |> IO.read_lines in
  begin
    (* PART 2 *)
    file
    |> map decode
    |> map to_seat
    |> map (fun (a, b) -> Option.(get a, get b))
    |> map to_sid
    |> sort Int.compare
    |> fold_left (fun a b ->
        match a with
        | Ok a -> Ok a
        | Error a -> if b = a+2 then Ok (a+1) else Error b) (Error 0)
    |> Result.get_ok
    |> print_int

  end

let () = Arg.parse [] main ""
