open Dev

let run_prog r0 ip prog =
  let regs = Array.make 6 0 in
  regs.(0) <- r0;
  let len = Array.length prog in
  let ns = ref [] in

  try
    while regs.(ip) < len do
      if regs.(ip) = 28 then begin
        if List.exists ((=) regs.(3)) !ns
        then raise Exit
        else ns := regs.(3) :: !ns;
        regs.(ip) <- 6;
      end
      else if regs.(ip) = 17 then begin (* partial optimization *)
        regs.(2) <- regs.(2) / 256;
        regs.(ip) <- 8;
      end
      else begin
        let op, args = prog.(regs.(ip)) in
        step op regs args;
        regs.(ip) <- regs.(ip) + 1;
      end;
    done;
    []
  with Exit -> List.rev !ns

let () =
  let ip, prog =
    open_in Sys.argv.(1)
    |> In_channel.input_all
    |> String.trim
    |> String.split_on_char '\n'
    |> parse
  in
  let ns = run_prog 0 ip prog in
  (* PART 1 *)
  print_int @@ List.nth ns 0;
  print_newline ();

  (* PART 2 *)
  print_int @@ List.(nth ns @@ length ns - 1)
