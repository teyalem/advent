open Ut
open List

let least_fuel f ns =
  sort_uniq Int.compare ns
  |> map (fun n -> sum @@ map (f n) ns)
  |> fold_left min Int.max_int

let () =
  let data =
    open_in Sys.argv.(1) |> IO.input_all
    |> String.trim |> String.split_on_char ','
    |> map int_of_string
  in
  begin
    (* PART 1 *)
    let diff n k = abs (k - n) in
    least_fuel diff data
    |> Printf.printf "%d\n";

    (* PART 2 *)
    let sigma n = n*(n+1)/2 in
    least_fuel (fun n k -> sigma @@ diff n k) data
    |> Printf.printf "%d\n"
  end
