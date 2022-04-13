open Ut

let parse str =
  Scanf.sscanf str "%s %s %s" (fun cmd x y -> cmd, x, y)

type comp = {
  mutable ip : int;
  regs : int array;
  prog : (string * string * string) array;
}

let make prog =
  { ip = 0;
    regs = Array.make 4 0;
    prog = Array.of_list prog; }

let r2i = function
  | "a" -> 0
  | "b" -> 1
  | "c" -> 2
  | "d" -> 3
  | v -> Printf.printf "[%s]\n" v; assert false

let get m r = m.regs.(r2i r)
let geti m x = try int_of_string x with _ -> get m x
let set m r v = m.regs.(r2i r) <- v

let step m =
  if m.ip >= Array.length m.prog then false
  else begin
    let cmd, x, y = m.prog.(m.ip) in
    (match cmd with
     | "cpy" -> set m y @@ geti m x; m.ip <- m.ip + 1
     | "inc" -> set m x @@ get m x + 1; m.ip <- m.ip + 1
     | "dec" -> set m x @@ get m x - 1; m.ip <- m.ip + 1
     | "jnz" -> m.ip <- m.ip + if geti m x <> 0 then int_of_string y else 1
     | _ -> assert false);
    true
  end

let run m = while step m do () done

let () =
  let data = IO.read_lines () |> List.map parse in
  begin
    (* PART 1 *)
    let m = make data in
    run m;
    get m "a" |> Printf.printf "%d\n";

    (* PART 2 *)
    let m = make data in
    set m "c" 1;
    run m;
    get m "a" |> Printf.printf "%d\n";
  end
