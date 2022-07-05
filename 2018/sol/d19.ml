open Dev

let run_prog r0 ip prog =
  let regs = Array.make 6 0 in
  regs.(0) <- r0;
  let len = Array.length prog in
  while regs.(ip) < len && regs.(ip) <> 1 do
    let op, args = prog.(regs.(ip)) in
    step op regs args;
    regs.(ip) <- regs.(ip) + 1;
  done;
  let t = ref 0 in
  for a = 1 to regs.(5) do
    if regs.(5) mod a = 0 then t := !t + a
  done;
  !t

let () =
  let ip, prog =
    open_in Sys.argv.(1)
    |> In_channel.input_all
    |> String.trim
    |> String.split_on_char '\n'
    |> parse
  in
  (* PART 1 *)
  run_prog 0 ip prog |> print_int;
  print_newline();
  (* PART 2 *)
  run_prog 1 ip prog |> print_int;
