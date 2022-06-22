module M = Map.Make(Int)
module S = Set.Make(String)

let cmds = [
  "addr"; "addi";
  "mulr"; "muli";
  "banr"; "bani";
  "borr"; "bori";
  "setr"; "seti"; 
  "gtir"; "gtri"; "gtrr";
  "eqir"; "eqri"; "eqrr";
]

let rrop op regs (a, b, c) =
  regs.(c) <- op regs.(a) regs.(b)

let riop op regs (a, b, c) =
  regs.(c) <- op regs.(a) b

let irop op regs (a, b, c) =
  regs.(c) <- op a regs.(b)

let bool_to_int = function
  | false -> 0
  | true -> 1

let gt a b = bool_to_int (a > b)
let eq a b = bool_to_int (a = b)

let step cmd =
  match cmd with
  | "addr" -> rrop Int.add
  | "addi" -> riop Int.add
  | "mulr" -> rrop Int.mul
  | "muli" -> riop Int.mul
  | "banr" -> rrop Int.logand
  | "bani" -> riop Int.logand
  | "borr" -> rrop Int.logor
  | "bori" -> riop Int.logor
  | "setr" -> rrop Fun.const
  | "seti" -> irop Fun.const
  | "gtir" -> irop gt
  | "gtri" -> riop gt
  | "gtrr" -> rrop gt
  | "eqir" -> irop eq
  | "eqri" -> riop eq
  | "eqrr" -> rrop eq
  | _ -> assert false

let test before after args cmd =
  let before = Array.copy before in
  step cmd before args;
  before = after

let possible_ops (before, (_, args), after) =
  cmds
  |> List.filter (test before after args)
  |> S.of_list

let collect =
  let some = Option.some in
  List.fold_left (fun bag (_, (op, _), _ as t) ->
      let ops = possible_ops t in
      bag |> M.update op
        (function | None -> some ops | Some s -> some @@ S.inter s ops))
    M.empty

let rec place_ops bag =
  let fixed, rest = M.partition (fun _ s -> S.cardinal s = 1) bag in
  let fixed = M.map S.choose fixed in
  if M.cardinal rest = 0 then
    fixed
  else
    let fixed_ops = M.to_seq fixed |> Seq.map snd |> S.of_seq in
    let rest = M.map (fun s -> S.diff s fixed_ops) rest in
    M.union (fun _ a _ -> Some a) fixed (place_ops rest)

let translate_from_samples samples prog =
  let bag = collect samples |> place_ops in
  List.map (fun (op, args) -> M.find op bag, args) prog

let print_op (op, (a, b, c)) =
  Printf.printf "%s %d %d %d\n" op a b c

let run_prog prog =
  let regs = Array.make 4 0 in
  List.iter (fun (op, args) -> step op regs args) prog;
  regs.(0)

let parse_op str =
  Scanf.sscanf str "%d %d %d %d" (fun a b c d -> a, (b, c, d))

let parse_arr str =
  Scanf.sscanf str "%s@: [%d, %d, %d, %d]"
    (fun _ a b c d -> [|a; b; c; d|])

let read lines =
  let rec aux samples = function
    | a::b::c::""::xs when String.starts_with ~prefix: "Before:" a ->
        aux ((parse_arr a, parse_op b, parse_arr c) :: samples) xs
    | ""::""::xs -> samples, List.map parse_op xs
    | _ -> assert false
  in
  let samples, test_prog = aux [] lines in
  List.rev samples, test_prog

let () = Printexc.record_backtrace true
let () =
  let samples, test_prog =
    open_in Sys.argv.(1)
    |> In_channel.input_all
    |> String.trim
    |> String.split_on_char '\n'
    |> read
  in
  (* PART 1 *)
  List.map possible_ops samples
  |> List.filter (fun s -> S.cardinal s >= 3)
  |> List.length
  |> print_int;
  print_newline ();

  (* PART 2 *)
  translate_from_samples samples test_prog
  |> run_prog
  |> print_int
