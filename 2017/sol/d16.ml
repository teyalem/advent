open Ut

type action =
  | Spin of int
  | Exchange of int * int
  | Partner of char * char

let parse str =
  let sc = Scanf.sscanf in
  match str.[0] with
  | 's' -> sc str "s%d" (fun n -> Spin n)
  | 'x' -> sc str "x%d/%d" (fun a b -> Exchange (a, b))
  | 'p' -> sc str "p%c/%c" (fun a b -> Partner (a, b))
  | _ -> assert false

let apply str =
  let len = String.length str in
  function
  | Spin n -> String.init len (fun i ->
      if i < n then str.[len - n + i] else str.[i - n])
  | Exchange (a, b) -> String.init len (fun i ->
      if i = a then str.[b]
      else if i = b then str.[a]
      else str.[i])
  | Partner (a, b) -> str |> String.map (fun c ->
      if c = a then b
      else if c = b then a
      else c)

let dance insts progs =
  Seq.fold_left apply progs insts

let find_loop insts progs =
  let rec aux ps p =
    if List.mem p ps then ps
    else aux (p::ps) @@ dance insts p
  in
  List.rev @@ aux [] progs

let progs = String.init 16 (fun i -> Char.(chr @@ code 'a' + i))
let billion = 1_000_000_000

let () =
  let data =
    IO.read_all ()
    |> String.split_on_char ','
    |> List.map parse
    |> List.to_seq
  in
  (* PART 1 *)
  Seq.fold_left apply progs data
  |> Printf.printf "%s\n";

  (* PART 2 *)
  let loop = find_loop data progs in
  let len = List.length loop in
  List.nth loop (billion mod len)
  |> Printf.printf "%s\n";
