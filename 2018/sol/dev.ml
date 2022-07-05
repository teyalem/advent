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
  | "setr" -> riop Fun.const
  | "seti" -> iiop Fun.const
  | "gtir" -> irop gt
  | "gtri" -> riop gt
  | "gtrr" -> rrop gt
  | "eqir" -> irop eq
  | "eqri" -> riop eq
  | "eqrr" -> rrop eq
  | _ -> assert false

let print_op (op, (a, b, c)) =
  Printf.printf "%s %d %d %d\n" op a b c

let parse_ip str =
  Scanf.sscanf str "#ip %d" Fun.id

let parse_op str =
  Scanf.sscanf str "%s %d %d %d" (fun a b c d -> a, (b, c, d))

let parse = function
  | [] -> failwith "no prog"
  | ip::prog ->
    parse_ip ip, List.map parse_op prog |> Array.of_list
