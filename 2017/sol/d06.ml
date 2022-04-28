open Ut

let realloc mem =
  let len = Array.length mem in
  let mi, mm =
    Array.to_seqi mem
    |> Seq.fold_left (fun (_, mn as m) (_, nn as n) ->
        if nn > mn then n else m)
      (-1, min_int)
  in
  let mem = Array.mapi (fun i n -> (if i = mi then 0 else n) + mm / len) mem in
  Seq.init (mm mod len) (fun i -> (i + mi + 1) mod len)
  |> Seq.iter (fun i -> mem.(i) <- mem.(i) + 1);
  mem

let count_cycle mem =
  let rec aux ms i m =
    if List.mem m ms then i, m
    else aux (m::ms) (i+1) (realloc m)
  in
  let i, m = aux [] 0 @@ Array.of_list mem in
  let j, _ = aux [] 0 m in
  i, j

let () =
  let data =
    IO.read_all ()
    |> String.split_on_char '\t'
    |> List.map int_of_string
  in
  let i, j = count_cycle data in
  (* PART 1 *) Printf.printf "%d\n" i;
  (* PART 2 *) Printf.printf "%d\n" j
