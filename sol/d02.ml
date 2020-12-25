open Advent

module IntCode = struct
  type t = { mutable pc: int;
             mem: int array; }

  let load ns = { pc = 0;
                  mem = Array.of_list ns; }
  
  let get m i = m.mem.(i)
  let set m i n = m.mem.(i) <- n

  let iget m i = get m @@ get m i
  let iset m i n = set m (get m i) n

  let fetch m = m.mem.(m.pc)

  let indirect_op f m =
      let s1 = iget m (m.pc + 1)
      and s2 = iget m (m.pc + 2) in
      iset m (m.pc + 3) (f s1 s2)

  (* run a step of an IntCode Computer. returns offset for program counter to
   * move. *)
  let step m =
    match fetch m with
    | 1 -> indirect_op Int.add m; 4 (* ADD *)
    | 2 -> indirect_op Int.mul m; 4 (* MUL *)
    | 99 -> -1 (* Program Termination *)
    | _ -> raise (Invalid_argument "step")

  let run m =
    let break = ref false in
    while not !break do
      let off = step m in
      if off = -1
      then break := true
      else m.pc <- m.pc + off
    done

end

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
