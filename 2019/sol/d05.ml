open Ut

let () =
  let data = IO.read_all () |> IntCode.parse_code in
  begin
    (* PART 1 *)
    let m = IntCode.load data in
    IntCode.push_input m 1;
    IntCode.run m;
    IntCode.pop_output m |> print_int;

    print_newline ();

    (* PART 2 *)
    let m = IntCode.load data in
    IntCode.push_input m 5;
    IntCode.run m;
    IntCode.pop_output m |> print_int
  end
