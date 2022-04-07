open Ut

let check_pos code x y =
  let m = IntCode.load code in
  IntCode.push_input m x;
  IntCode.push_input m y;
  IntCode.run m;
  IntCode.pop_output m = 1

let () =
  let data = IO.read_all () |> IntCode.parse_code in
  let check_pos = check_pos data in
  begin
    (* PART 1 *)
    let c = ref 0 in
    for x = 0 to 49 do
      for y = 0 to 49 do
        if check_pos x y then incr c
      done
    done;
    Printf.printf "%d\n" !c;

    (* PART 2 *)
    (* credit to /u/4HbQ
     * https://www.reddit.com/r/adventofcode/comments/ecogl3/comment/fbdmn5n *)
    let x = ref 0 and y = ref 0 in
    while not @@ check_pos (!x+99) !y do
      incr y;
      while not @@ check_pos !x (!y+99) do
        incr x
      done
    done;

    let x, y = !x, !y in
    Printf.printf "%d\n" (x * 10000 + y)
  end
