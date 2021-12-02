open Ut

(* overengineered a bit *)
module Submarine = struct
  type t = {
    mutable horz : int;
    mutable depth : int;
    mutable aim : int;
  }

  let make () = {
    horz = 0;
    depth = 0;
    aim = 0;
  }

  let do_action1 z (w, n) =
    match w with
    | "forward" -> z.horz <- z.horz + n
    | "down" -> z.depth <- z.depth + n
    | "up" -> z.depth <- z.depth - n
    | _ -> assert false

  let do_action2 z (w, n) =
    match w with
    | "forward" ->
      z.horz <- z.horz + n;
      z.depth <- z.depth + z.aim * n
    | "down" -> z.aim <- z.aim + n
    | "up" -> z.aim <- z.aim - n
    | _ -> assert false
end

let parse ss =
  List.map (fun s ->
      match String.split_on_char ' ' s with
      | [w; n] -> w, int_of_string n
      | _ -> assert false)
    ss

let () =
  let data = open_in Sys.argv.(1) |> IO.input_lines |> parse in
  begin
    let z = Submarine.make () in
    List.iter (Submarine.do_action1 z) data;
    Printf.printf "%d\n" (z.horz * z.depth);

    let z = Submarine.make () in
    List.iter (Submarine.do_action2 z) data;
    Printf.printf "%d\n" (z.horz * z.depth)
  end
