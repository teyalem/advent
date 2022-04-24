open Ut

let captcha ns =
  let rec aux v0 acc = function
    | [] -> acc
    | [n] -> acc + if n = v0 then n else 0
    | n::m::ns ->
      let acc = acc + if n = m then n else 0 in
      aux v0 acc (m::ns)
  in
  aux (List.hd ns) 0 ns

let captcha2 ns =
  let open Array in
  let arr = of_list ns in
  let len = length arr in
  init len (fun i ->
      if arr.(i) = arr.((i + len/2) mod len)
      then arr.(i)
      else 0)
  |> fold_left Int.add 0

let () =
  let data =
    IO.read_all ()
    |> String.to_seq
    |> Seq.map (fun c -> Char.(code c - code '0'))
    |> List.of_seq
  in
  (* PART 1 *) captcha data |> Printf.printf "%d\n";
  (* PART 2 *) captcha2 data |> Printf.printf "%d\n";
