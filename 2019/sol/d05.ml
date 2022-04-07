open Ut

let main path =
  let data = open_in path |> IO.read_file |> IntCode.parse_code in
  begin
    (* PART 1 *)
    let m = IntCode.load data in
    IntCode.set_input m 1;
    IntCode.run m;
    IntCode.get_output m |> print_int;

    print_newline ();

    (* PART 2 *)
    let m = IntCode.load data in
    IntCode.set_input m 5;
    IntCode.run m;
    IntCode.get_output m |> print_int
  end

let () = Arg.parse [] main ""
