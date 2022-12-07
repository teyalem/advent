type rps = R | P | S

let to_rps = function
  | 'A' | 'X' -> R
  | 'B' | 'Y' -> P
  | 'C' | 'Z' -> S
  | _ -> assert false

let parse str =
  Scanf.sscanf str "%c %c" (fun a b -> a, b)

let shape_score = function
  | R -> 1
  | P -> 2
  | S -> 3

let win_shape = function
  | R -> P
  | S -> R
  | P -> S

let lose_shape = function
  | R -> S
  | S -> P
  | P -> R

let score1 (a, b) =
  let s = shape_score b in
  if b = win_shape a then 6 + s
  else if a = b then 3 + s
  else s

let score2 (a, action) =
  match action with
  | 'X' (* lose *) -> shape_score @@ lose_shape a
  | 'Y' (* draw *) -> 3 + shape_score a
  | 'Z' (* win *) -> 6 + shape_score (win_shape a)
  | _ -> assert false

let () =
    let data =
      open_in Sys.argv.(1)
      |> In_channel.input_all
      |> String.split_on_char '\n'
      |> List.filter ((<>) "")
      |> List.map parse
    in
    (* PART 1 *)
    List.map (fun (a, b) -> to_rps a, to_rps b) data
    |> List.map score1
    |> List.fold_left (+) 0
    |> print_int;
    print_newline ();

    (* PART 2 *)
    List.map (fun (a, b) -> to_rps a, b) data
    |> List.map score2
    |> List.fold_left (+) 0
    |> print_int
