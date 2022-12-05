open Ut

let parse_com str =
  Scanf.sscanf str "move %d from %d to %d" (fun n f t -> n, f, t)

let parse_stack ss =
  let aux n =
    List.map (fun s -> s.[n]) ss
    |> List.filter ((<>) ' ')
  in
  let n =
    List.map String.length ss
    |> List.fold_left max min_int
    |> (fun n -> n/4 + 1)
  in
  List.init n (fun i -> aux (1 + 4*i))

let parse ss =
  let rec aux buf = function
    | [] -> assert false
    | _ :: "" :: xs -> (* drop stack numbers *)
      Array.of_list @@ parse_stack @@ List.rev buf,
      List.map parse_com xs
    | x :: xs -> aux (x::buf) xs
  in
  aux [] ss

let move is_part2 stack (n, f, t) =
  let rec split i = function
    | [] -> if i > 0 then assert false else [], []
    | x::xs ->
      if i = 0 then [], x::xs
      else
        let f, s = split (i-1) xs in
        x::f, s
  in
  let moved, rest = split n stack.(f-1) in
  let revf = if is_part2 then Fun.id else List.rev in
  stack.(f-1) <- rest;
  stack.(t-1) <- revf moved @ stack.(t-1);
  stack

let solve is_part2 stack com =
  List.fold_left (move is_part2) (Array.copy stack) com
  |> Array.map List.hd
  |> Array.iter print_char

let () =
  let stack, com = open_in Sys.argv.(1) |> IO.input_lines |> parse in
  solve false stack com;
  print_newline ();
  solve true stack com
