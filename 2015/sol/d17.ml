open Ut

let combis amount cons =
  let rec aux rem cons = function
    | [] -> if rem = 0 then [cons] else []
    | n::ns ->
      if n > rem then
        aux rem cons ns
      else
        aux (rem-n) (n::cons) ns @ aux rem cons ns
  in
  aux amount [] cons

let () =
  let data = IO.read_lines () |> List.map int_of_string in
  let open List in
  let conss = combis 150 data in
  begin
    (* PART 1 *)
    length conss |> Printf.printf "%d\n";

    (* PART 2 *)
    let least = map length conss |> fold_left min max_int in
    conss
    |> filter (fun ns -> List.length ns = least)
    |> length
    |> Printf.printf "%d\n";
  end
