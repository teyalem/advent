open Ut

let () =
  let data = IO.read_all () |> IntCode.parse_code in
  begin
    (* PART 1 *)
    let m = IntCode.load data in
    let prog = (* '(ABC)D *)
      [ "OR A T";
        "AND B T";
        "AND C T";
        "NOT T J";
        "AND D J";
        "WALK\n"; ]
    in
    String.iter (fun c -> IntCode.push_input m @@ int_of_char c) @@ String.concat "\n" prog;
    IntCode.run m;
    IntCode.flush_ascii m |> print_int;

    print_newline ();

    (* PART 2 *)
    let m = IntCode.load data in
    let prog =
      [ "OR A T"; (* T = '(ABC) = 'A + 'B + 'C *)
        "AND B T";
        "AND C T";
        "NOT T T";

        "OR E J";
        "OR H J"; (* J = (E+H)D = ('E->H)D *)
        "AND D J";

        "AND T J"; (* TJ *)

        "RUN\n"; ]
    in
    String.iter (fun c -> IntCode.push_input m @@ int_of_char c) @@ String.concat "\n" prog;
    IntCode.run m;
    IntCode.flush_ascii m |> print_int;
  end
