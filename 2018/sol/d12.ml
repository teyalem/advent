module S = Set.Make(Int) (* set of indexes *)

type rule = bool array * bool

let fifty_billion = 50_000_000_000

let bool_of_char = function
  | '#' -> true
  | '.' -> false
  | _ -> assert false

let parse_state str =
  Scanf.sscanf str "initial state: %s" (fun st ->
      String.to_seq st
      |> Seq.map bool_of_char
      |> Seq.mapi (fun i b -> i, b)
      |> Seq.filter_map (fun (i, b) -> if b then Some i else None)
      |> S.of_seq)

let parse_rule str =
  Scanf.sscanf str "%s => %c" (fun i o ->
      String.to_seq i |> Seq.map bool_of_char |> Array.of_seq,
      bool_of_char o)

let parse = function
  | s :: "" :: rules ->
    parse_state s, List.map parse_rule rules
  | _ -> assert false

let neigh pots i =
  Array.init 5 (fun j -> i + j - 2)
  |> Array.map (fun i -> S.mem i pots)

let apply rules neigh =
  List.assoc_opt neigh rules
  |> Option.value ~default: false

let grow rules pots =
  let mini, maxi = S.min_elt pots, S.max_elt pots in
  Seq.init (maxi - mini + 5) (fun i -> i + mini - 2) (* mini-2 : maxi+2 *)
  |> Seq.map (fun i -> i, neigh pots i)
  |> Seq.filter_map (fun (i, n) ->
      if apply rules n then Some i else None)
  |> S.of_seq

let potsum s =
  S.fold Int.add s 0

let after_ngen rules pots n =
  Seq.iterate (grow rules) pots
  |> Seq.drop n
  |> Seq.uncons
  |> Option.get
  |> fst

let () =
  let pots, rules =
    In_channel.input_all stdin
    |> String.trim
    |> String.split_on_char '\n'
    |> parse
  in
  (* PART 1 *)
  after_ngen rules pots 20 |> potsum |> print_int;
  print_newline ();

  (* PART 2 *)
  let far = after_ngen rules pots 2000 in
  let farsum = potsum far in
  let diff = potsum (grow rules far) - farsum in
  (fifty_billion - 2000) * diff + farsum |> print_int;
