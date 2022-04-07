open Ut

let first_code = 20151125

let next_code c =
  (c * 252533) mod 33554393

let codes = Seq.iterate next_code first_code

let tri n =
  n*(n+1)/2

let find_pos r c =
  tri c + tri (r-1) + (r-1)*(c-1)

let parse str =
  Scanf.sscanf str "To continue, please consult the code grid in the manual.  Enter the code at row %d, column %d."
    (fun row col -> row, col)

let find_code row col =
  Seq.drop (find_pos row col - 1) codes
  |> Seq.uncons
  |> Option.map fst

let () =
  let row, col = IO.read_all () |> parse in
  (* PART 1 *)
  find_code row col |> Option.iter (Printf.printf "%d\n")
