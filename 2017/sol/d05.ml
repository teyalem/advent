open Ut

let count_jump f prog =
  let prog = Array.of_list prog in
  let rec aux jc i =
    if i < Array.length prog then begin
      let jp = i + prog.(i) in
      prog.(i) <- f prog.(i);
      aux (jc + 1) jp
    end
    else jc
  in
  aux 0 0

let () =
  let data = IO.read_lines () |> List.map int_of_string in
  (* PART 1 *)
  count_jump succ data |> Printf.printf "%d\n";
  (* PART 2 *)
  count_jump (fun n -> if n >= 3 then n - 1 else n + 1) data
  |> Printf.printf "%d\n";
