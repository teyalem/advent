open Ut

let check_pos code x y =
  let m = IntCode.load code in
  IntCode.set_input m x;
  IntCode.set_input m y;
  IntCode.run m;
  IntCode.get_output m = 1

let main path =
  let data = open_in path |> IO.read_file |> IntCode.parse_code in
  begin
    let check_pos = check_pos data in

    (* PART 1 *)
    let c = ref 0 in
    for x = 0 to 49 do
      for y = 0 to 49 do
        if check_pos x y
        then incr c
        else ()
      done
    done;
    print_int !c;

    print_newline ();

    (* PART 2 *)
    (* SLOW. is there any shortcut? *)
    let pos = ref (0, 0) in
    try
      for x = 100 to 10000 do
        for y = 100 to 10000 do
          if check_pos x y
          && check_pos (x+99) y
          && check_pos x (y+99)
          then begin
            pos := (x, y);
            raise Exit
          end
          else ()
        done
      done
    with Exit -> ();

    !pos
    |> (fun (x, y) -> Printf.printf "%d" (x * 10000 + y))

  end

let () = Arg.parse [] main ""
