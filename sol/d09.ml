open Advent

let main path =
  let data = open_in path |> IO.read_file |> IntCode.parse_code in
  begin
    (* PART 1 *)
    let open IntCode in
    let m = load data in
    set_input m 1;
    run m;
    get_output m |> print_int;

    print_newline ();

    (* PART 2 *)
    let m = load data in
    set_input m 2;
    run m;
    get_output m |> print_int
  end

let () = Arg.parse [] main ""
