(* from d16.ml *)
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

let iiop op regs (a, b, c) =
  regs.(c) <- op a b

let bool_to_int = function
  | false -> 0
  | true -> 1

let gt a b = bool_to_int (a > b)
let eq a b = bool_to_int (a = b)

let step = function
  | "addr" -> rrop Int.add
  | "addi" -> riop Int.add
  | "mulr" -> rrop Int.mul
  | "muli" -> riop Int.mul
  | "banr" -> rrop Int.logand
  | "bani" -> riop Int.logand
  | "borr" -> rrop Int.logor
  | "bori" -> riop Int.logor
  | "setr" -> rrop Fun.const
  | "seti" -> iiop Fun.const
  | "gtir" -> irop gt
  | "gtri" -> riop gt
  | "gtrr" -> rrop gt
  | "eqir" -> irop eq
  | "eqri" -> riop eq
  | "eqrr" -> rrop eq
  | _ -> assert false

let run_prog r0 ip prog =
  let regs = Array.make 6 0 in
  regs.(0) <- r0;
  let len = Array.length prog in
  while regs.(ip) < len && regs.(ip) <> 1 do
    let op, args = prog.(regs.(ip)) in
    step op regs args;
    regs.(ip) <- regs.(ip) + 1;
  done;
  let t = ref 0 in
  for a = 1 to regs.(5) do
    if regs.(5) mod a = 0 then t := !t + a
  done;
  !t

let parse_ip str =
  Scanf.sscanf str "#ip %d" Fun.id

let parse_op str =
  Scanf.sscanf str "%s %d %d %d" (fun a b c d -> a, (b, c, d))

let parse = function
  | [] -> failwith "no prog"
  | ip::prog ->
    parse_ip ip, List.map parse_op prog |> Array.of_list

let () = Printexc.record_backtrace true
let () =
  let ip, prog =
    open_in Sys.argv.(1)
    |> In_channel.input_all
    |> String.trim
    |> String.split_on_char '\n'
    |> parse
  in
  (* PART 1 *)
  run_prog 0 ip prog |> print_int;
  print_newline();
  (* PART 2 *)
  run_prog 1 ip prog |> print_int;
