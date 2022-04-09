open Ut

let () =
  let data = IO.read_all () |> IntCode.parse_code in
  begin
    (* PART 1 *)
    let m = IntCode.load data in
    IntCode.(
      poke m 1 12;
      poke m 2 2;
      run m;
      peek m 0 |> print_int);

    print_newline ();

    (* PART 2 *)
    try
      for noun = 0 to 99 do
        for verb = 0 to 99 do
          let m = IntCode.load data in
          IntCode.(poke m 1 noun;
                   poke m 2 verb;
                   run m);
          if IntCode.peek m 0 <> 19690720
          then ()
          else begin
            print_int (100 * noun + verb);
            raise Exit
          end
        done
      done
    with Exit -> ()

  end
