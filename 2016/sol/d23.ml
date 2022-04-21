open Ut

let parse str =
  Scanf.sscanf str "%s %s %s" (fun cmd x y -> cmd, x, y)

type comp = {
  mutable ip : int;
  mutable regs : int array;
  prog : (string * string * string) array;
}

let make_aux prog =
  { ip = 0;
    regs = Array.make 4 0;
    prog = prog; }

let make prog =
  make_aux @@ Array.of_list prog

let r2i = function
  | "a" -> 0
  | "b" -> 1
  | "c" -> 2
  | "d" -> 3
  | v -> Printf.printf "r2i: %s\n" v; assert false

let get m r = m.regs.(r2i r)
let geti m x = try int_of_string x with _ -> get m x
let set m r v = m.regs.(r2i r) <- v

let toggle = function
  | "inc" -> "dec"
  | "dec" -> "inc"
  | "tgl" -> "inc"
  | "jnz" -> "cpy"
  | "cpy" -> "jnz"
  | v -> Printf.eprintf "toggle: %s\n" v; assert false

let tick m = m.ip <- m.ip + 1

let is_reg x = 'a' <= x.[0] && x.[0] <= 'd'

let is_valid cmd x y =
  match cmd with
  | "cpy" -> is_reg y
  | "inc" | "dec" -> is_reg x
  | _ -> true

let rec step m =
  if m.ip >= Array.length m.prog then false
  else begin
    let cmd, x, y = m.prog.(m.ip) in
    if is_valid cmd x y then
      (match cmd with
       | "cpy" -> set m y @@ geti m x ; tick m
       | "inc" -> set m x @@ get m x + 1 ; tick m
       | "dec" -> set m x @@ get m x - 1 ; tick m
       | "jnz" ->
         let y = geti m y in
         if is_reg x && y < 0 then begin (* loop opt. *)
           if geti m x <> 0 then begin
             (* get subprogram *)
             let s = m.ip + y in
             let subp = Array.sub m.prog s ~-y in

             (* run machine with registers and subprogram *)
             let subm = make_aux subp in
             subm.regs <- Array.copy m.regs;
             run subm;

             (* figure out diff and how many times it should run *)
             let rdiff = Array.map2 Int.sub subm.regs m.regs in
             let diff = ~-(rdiff.(r2i x)) in
             let times = get m x / diff in

             (* apply register change *)
             Array.iteri (fun i n -> m.regs.(i) <- m.regs.(i) + times * n) rdiff;
           end;

           tick m
         end
         else (* normal branching *)
           m.ip <- m.ip + if geti m x <> 0 then y else 1
       | "tgl" ->
         let i = m.ip + geti m x in
         if i < Array.length m.prog then begin
           let cmd, x, y = m.prog.(i) in
           m.prog.(i) <- toggle cmd, x, y
         end;
         tick m
       | "nop" -> tick m
       | "add" -> set m x @@ get m x + get m y; tick m
       | _ -> assert false)
    else tick m (* skip *);
    true
  end

and run m =
  while step m do () done

let () =
  let data = IO.read_lines () |> List.map parse in
  let f n =
    let m = make data in
    set m "a" n;
    run m;
    get m "a" |> Printf.printf "%d\n";
  in
  (* PART 1 *) f 7;
  (* PART 2 *) f 12
