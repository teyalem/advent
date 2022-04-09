open Ut

let needed_fuel mass = mass/3 - 2

let sum_needed_fuel mass =
  let rec loop tmp =
    if tmp < 0
    then []
    else tmp :: (loop @@ needed_fuel tmp)
  in
  sum @@ loop @@ needed_fuel mass

let () =
  let data = IO.read_lines () |> List.map int_of_string in
  begin
    (* PART 1 *)
    List.map needed_fuel data |> sum |> print_int;

    print_newline ();

    (* PART 2 *)
    List.map sum_needed_fuel data |> sum |> print_int;
  end
