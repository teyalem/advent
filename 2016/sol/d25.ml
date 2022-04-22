open Ut

let parse str =
  Scanf.sscanf str "%s %s %s" (fun cmd x y -> cmd, x, y)

let (let*) = Option.bind

let solve = function
  | _ :: ("cpy", a, "c") :: ("cpy", b, "b") :: _ ->
    let* a = int_of_string_opt a in
    let* b = int_of_string_opt b in
    Some (0xaaa - a*b)
  | _ -> None

let () =
  let data = IO.read_lines () |> List.map parse in
  (* PART 1 *)
  solve data |> Option.iter (Printf.printf "%d\n")
