open Ut
open List

let count =
  fold_left (fun (z, o) c ->
      match c with
      | '0' -> z+1, o
      | '1' -> z, o+1
      | _ -> assert false)
    (0, 0)

let gather_at ss i =
  map (fun s -> s.[i]) ss |> count

let gamma_rate ss =
  init (String.length @@ hd ss) (gather_at ss)
  |> map (fun (z, o) -> if z > o then '0' else '1')

let flip =
  map (function
      | '0' -> '1'
      | '1' -> '0'
      | _ -> assert false)

let gas_rate f ss =
  let l = String.length @@ hd ss in
  let rec aux i ss =
    if i = l || length ss = 1
    then hd ss
    else
      let bit = f @@ gather_at ss i in
      aux (i+1) @@ filter (fun s -> s.[i] = bit) ss
  in
  aux 0 ss |> String.to_seq |> of_seq

let oxygen_rate =
  gas_rate (fun (z, o) -> if z > o then '0' else '1')

let co2_rate =
  gas_rate (fun (z, o) -> if z <= o then '0' else '1')

let to_num cs =
  cs
  |> map (function '0' -> 0 | '1' -> 1 | _ -> assert false)
  |> fold_left (fun n d -> 2*n + d) 0

let () =
  let data = IO.read_lines () in
  begin
    (* PART 1 *)
    let gr = gamma_rate data in
    let er = flip gr in
    Printf.printf "%d\n" (to_num gr * to_num er);

    (* PART 2 *)
    let ox = oxygen_rate data
    and co = co2_rate data in
    Printf.printf "%d\n" (to_num ox * to_num co)
  end
