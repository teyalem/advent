(* Complete IntCode VM *)

(* states of VM *)
type state = Running (* Running normally *)
           | Paused (* Pause for some reason (e.g. program is in need of input) *)
           | Halt (* Program ended *)

(* IntCode VM *)
type t = { mutable pc: int; (* instruction pointer *)
           mutable rel_base: int; (* relative base register *)
           mutable state: state; (* state indicator *)
           mutable is_interactive: bool; (* interactive mode switch *)
           input: int Queue.t; (* Input queue *)
           output: int Queue.t; (* Output queue *)
           mem: int array; (* program memory *)
           ex_mem: (int, int) Hashtbl.t; } (* extended memory *)

type program = int list

(* make machine and load program *)
let load ns = { pc = 0;
                rel_base = 0;
                input = Queue.create ();
                state = Paused;
                is_interactive = false;
                output = Queue.create ();
                mem = Array.of_list ns;
                ex_mem = Hashtbl.create 100; }

(* copy machine *)
let copy m = { pc = m.pc;
               rel_base = m.rel_base;
               input = Queue.copy m.input;
               state = m.state;
               is_interactive = m.is_interactive;
               output = Queue.create ();
               mem = Array.copy m.mem;
               ex_mem = Hashtbl.copy m.ex_mem; }

(* return true if machine is halted *)
let is_halt m = m.state = Halt

(* return length of loaded code *)
let code_length m = Array.length m.mem

let peek m i =
  if i < code_length m
  then m.mem.(i)
  else Hashtbl.find_opt m.ex_mem i |> Option.value ~default: 0

let poke m i v =
  if i < code_length m
  then m.mem.(i) <- v
  else Hashtbl.replace m.ex_mem i v

(* switch interactive mode *)
let set_interactive m b = m.is_interactive <- b

(*
 * I/O
 *)

let is_output_empty m = Queue.is_empty m.output

let push_input m n = Queue.push n m.input
let pop_output m = Queue.pop m.output

(* flush ASCII and return first non-ASCII output *)
let rec flush_ascii m =
  let o = pop_output m in
  if o > 127
  then o
  else flush_ascii m

let rec collect_output m =
  if is_output_empty m
  then []
  else pop_output m :: collect_output m

let print_output m =
  while not @@ is_output_empty m do
    print_char @@ char_of_int @@ pop_output m
  done

(* get/set *)
let get m i =
  if i < Array.length m.mem
  then m.mem.(i)
  else Hashtbl.find_opt m.ex_mem i
       |> Option.value ~default: 0

let set m i n =
  if i < Array.length m.mem
  then m.mem.(i) <- n
  else Hashtbl.add m.ex_mem i n

(* indirect get/set *)
let iget m i = get m @@ get m i
let iset m i n = set m (get m i) n

(* relative get/set *)
let rget m i = get m (get m i + m.rel_base)
let rset m i n = set m (get m i + m.rel_base) n

let fetch m = m.mem.(m.pc)

(* decode instruction *)
let decode inst =
  let op, inst = inst mod 100 , inst / 100 in
  let m1 = inst mod 10 in
  let m2 = inst mod 100  / 10 in
  let m3 = inst mod 1000 / 100 in
  op, m1, m2, m3

(* load integer from memory *)
let load_i m mode off =
  match mode with
  | 0 -> iget m (m.pc + off) (* position mode *)
  | 1 -> get m (m.pc + off) (* immediate mode *)
  | 2 -> rget m (m.pc + off) (* relative mode *)
  | _ -> assert false

(* store integer to memory *)
let store_i m mode off i =
  match mode with
  | 0 -> iset m (m.pc + off) i
  | 2 -> rset m (m.pc + off) i
  | _ -> assert false

let set_pc m i = m.pc <- i
let offset_pc m off = set_pc m (m.pc + off)

(* for four code *)
let execute_4 m f m1 m2 m3 =
  let s1 = load_i m m1 1
  and s2 = load_i m m2 2 in
  store_i m m3 3 @@ f s1 s2;
  offset_pc m 4

(* execute jump instruction *)
let jump m f m1 m2 =
  if f @@ load_i m m1 1
  then set_pc m @@ load_i m m2 2
  else offset_pc m 3

(* execute set compare instruction *)
let set_compare m f m1 m2 m3 =
  let s a b = if f a b then 1 else 0 in
  execute_4 m s m1 m2 m3

(* run a step of an IntCode Computer. returns offset for program counter to
 * move. *)
let step m =
  let inst = fetch m in
  let op, m1, m2, m3 = decode inst in
  match op with
  | 1 -> execute_4 m Int.add m1 m2 m3 (* ADD *)
  | 2 -> execute_4 m Int.mul m1 m2 m3 (* MUL *)

  | 3 -> (* Input *)
      if Queue.is_empty m.input
      then m.state <- Paused (* wait for input *)
      else begin
        let input = Queue.pop m.input in
        store_i m m1 1 input;
        offset_pc m 2
      end

  | 4 -> (* Output *)
    Queue.push (load_i m m1 1) m.output;
    offset_pc m 2

  | 5 -> jump m ((<>) 0) m1 m2 (* jump if true *)
  | 6 -> jump m ((=) 0) m1 m2 (* jump if false *)

  | 7 -> set_compare m (<) m1 m2 m3 (* less then *)
  | 8 -> set_compare m (=) m1 m2 m3 (* equals *)

  | 9 -> (* adjust relative base *)
    m.rel_base <- m.rel_base + load_i m m1 1;
    offset_pc m 2

  | 99 -> m.state <- Halt (* Program Termination *)
  | _ -> invalid_arg "Intcode.step"

let rec run m =
  m.state <- Running; (* resume machine *)

  while m.state = Running do
    step m;

    if m.is_interactive && not @@ is_output_empty m then
      let output = pop_output m in
      try char_of_int output |> print_char
      with _ -> begin
          m.state <- Halt;
          Queue.push output m.output
        end
  done;

  if m.is_interactive && m.state = Paused then begin
    read_line () ^ "\n"
    |> String.iter (fun c -> push_input m @@ int_of_char c);
    run m
  end

(* parse intcode *)
let parse_code str =
  Delim.split "[,\n]" str |> List.map int_of_string
