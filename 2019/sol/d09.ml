open Ut
open IntCode

let () =
  let data = IO.read_all () |> parse_code in
  begin
    (* PART 1 *)
    let m = load data in
    push_input m 1;
    run m;
    pop_output m |> print_int;

    print_newline ();

    (* PART 2 *)
    let m = load data in
    push_input m 2;
    run m;
    pop_output m |> print_int
  end
