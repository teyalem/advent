open Ut

let parse_buslist str =
  Delim.split "," str
  |> List.filter (fun n -> n <> "x")
  |> List.map int_of_string

(* PART 1 Solver *)
let find_earliest_bus start_min buslist =
  let ebus, depart_min =
    buslist |> List.map (fun n -> n, (start_min/n + 1) * n)
    |> List.fold_left
      (fun (pn, pmin) (n, emin) ->
         if pmin > emin then n, emin else pn, pmin)
      (0, Int.max_int)
  in
  ebus, depart_min - start_min

let main path =
  let data = open_in path |> IO.input_lines in
  let dpnum = List.nth data 0 |> int_of_string
  and buslist = List.nth data 1 |> parse_buslist
  in
  begin
    (* PART 1 *)
    let bus, wait_min = find_earliest_bus dpnum buslist in
    print_int (bus * wait_min);

    print_newline ();

    (* PART 2 *)
    let full_buslist = Delim.split "," (List.nth data 1) in
    let cong = full_buslist
               |> List.mapi (fun i n -> i, n)
               |> List.filter_map (fun (i, n) ->
                   if n = "x" then None
                   else
                     let n = int_of_string n in
                     Some (n, if i = 0 then 0 else (abs (~-i mod n)))
                 )
    in
    let t = Math.crt cong in
    print_int (abs t)
  end

let _ = Arg.parse [] main ""
