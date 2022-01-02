open Ut

type submarine = {
  horz : int;
  depth : int;
  aim : int;
}

let submarine = {
  horz = 0;
  depth = 0;
  aim = 0;
}

let do_action1 z (w, n) =
  match w with
  | "forward" -> { z with horz = z.horz + n }
  | "down" -> { z with depth = z.depth + n }
  | "up" -> { z with depth = z.depth - n }
  | _ -> assert false

let do_action2 z (w, n) =
  match w with
  | "forward" ->
    { z with horz = z.horz + n; depth = z.depth + z.aim * n }
  | "down" -> { z with aim = z.aim + n }
  | "up" -> { z with aim = z.aim - n }
  | _ -> assert false

let parse =
  List.map (fun s -> Scanf.sscanf s "%s %d" (fun w n -> w, n))

let print { horz; depth } =
  Printf.printf "%d\n" (horz * depth)

let () =
  let data = IO.read_lines () |> parse in
  let solve f = print @@ List.fold_left f submarine data in
  begin
    (* PART 1 *) solve do_action1;
    (* PART 2 *) solve do_action2;
  end
