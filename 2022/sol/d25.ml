open Ut

module Snafu = struct
  type t = char Seq.t

  let digit = function
    | '=' -> -2
    | '-' -> -1
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | _ -> assert false

  let of_string = String.to_seq
  let to_string = String.of_seq

  let to_int =
    Seq.fold_left (fun acc n -> acc * 5 + digit n) 0

  let of_int n =
    let rec aux acc n =
      if n = 0 then acc
      else
        let k =
          match n mod 5 with
          | 0 -> '0'
          | 1 -> '1'
          | 2 -> '2'
          | 3 -> '='
          | 4 -> '-'
          | _ -> assert false
        in
        aux (k::acc) (n/5 + if k = '=' || k = '-' then 1 else 0) 
    in
    List.to_seq @@ aux [] n
end

let () =
  let data = open_in Sys.argv.(1) |> IO.input_lines |> List.map Snafu.of_string in
  List.map Snafu.to_int data
  |> List.fold_left (+) 0
  |> Snafu.of_int
  |> Snafu.to_string
  |> print_endline
