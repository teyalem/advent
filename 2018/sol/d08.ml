let return = Option.some
let (let*) = Option.bind

type t = { children: t list; metadata: int list; }

let node children metadata = { children; metadata }

let rec entry f n seq =
  if n = 0 then return ([], seq)
  else
    let* x, seq = f seq in
    let* xs, seq = entry f (n-1) seq in
    return (x::xs, seq)

let rec parse (seq : int Seq.t) : ('a * int Seq.t) option =
  let* nc, seq = Seq.uncons seq in
  let* nm, seq = Seq.uncons seq in
  let* children, seq = entry parse nc seq in
  let* metadata, seq = entry Seq.uncons nm seq in
  return (node children metadata, seq)

let sum = List.fold_left Int.add 0

let rec sum_of_metadata { children; metadata } =
  sum metadata
  + (List.map sum_of_metadata children |> sum)

let rec node_value { children; metadata } =
  let len = List.length children in
  if len = 0 then sum metadata
  else
    List.filter (fun i -> i <> 0 && i < len+1) metadata
    |> List.map (fun i -> List.nth children (i-1))
    |> List.map node_value
    |> sum

let () =
  let data, _ =
    open_in Sys.argv.(1)
    |> In_channel.input_all
    |> String.trim
    |> String.split_on_char ' '
    |> List.map int_of_string
    |> List.to_seq
    |> parse
    |> Option.get
  in
  (* PART 1 *)
  data |> sum_of_metadata |> print_int;
  print_newline ();

  (* PART 2 *)
  data |> node_value |> print_int;
