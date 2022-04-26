open Ut

let cksum sheet =
  sheet
  |> List.map
    (List.fold_left (fun (h, l) n -> max h n, min l n) (min_int, max_int))
  |> List.map (fun (h, l) -> h - l)
  |> List.fold_left Int.add 0

let find_tuple ns =
  let rec aux = function
    | [] -> None
    | n::ns ->
      match List.find_opt (fun m -> m mod n = 0) ns with
      | None -> aux ns
      | Some m -> Some (n, m)
  in
  Option.get @@ aux @@ List.sort Int.compare ns

let evens sheet =
  sheet
  |> List.map find_tuple
  |> List.map (fun (n, m) -> m / n)
  |> List.fold_left Int.add 0


let () =
  let data =
    IO.read_lines ()
    |> List.map (fun s ->
        String.split_on_char '\t' s |> List.map int_of_string)
  in
  (* PART 1 *) cksum data |> Printf.printf "%d\n";
  (* PART 2 *) evens data |> Printf.printf "%d\n"
