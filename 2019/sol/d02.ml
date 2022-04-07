open Ut

let main path =
  let data = open_in path |> IO.read_file 
             |> Delim.split "[,\n]" 
             |> List.map int_of_string
  in
  begin
    (* PART 1 *)
    let m = IntCode.load data in
    IntCode.set m 1 12;
    IntCode.set m 2 2;
    IntCode.run m;
    IntCode.get m 0 |> print_int;

    print_newline ();

    (* PART 2 *)
    try
      for noun = 0 to 99 do
        for verb = 0 to 99 do
          let m = IntCode.load data in
          IntCode.set m 1 noun;
          IntCode.set m 2 verb;
          IntCode.run m;
          if IntCode.get m 0 <> 19690720
          then ()
          else begin
            print_int (100 * noun + verb);
            raise Exit
          end
        done
      done
    with Exit -> ()

  end

let () = Arg.parse [] main ""
