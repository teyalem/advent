open List

let file = "input01"

let fuel_needed mass = mass / 3 - 2

let total_fuel mass =
    let fuel = fuel_needed mass in
    if fuel <= 0 then 0
    else fuel + fuel_needed fuel

let read_file f =
    let rec read_numbers f =
        match input_line f with
        |exception End_of_file -> []
        |n -> (int_of_string n) :: (read_numbers f)
    in read_numbers f

let _ =
    let f = open_in file in
    let ns = read_file f in
    let fuels = map (total_fuel) ns in
    print_int (fold_left (fun a b -> a + b) 0 fuels);
    print_endline ""
