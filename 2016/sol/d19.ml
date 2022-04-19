let elfs = 3001330

(* A006257. *)
let elephant n =
  let rec aux k = (* highest bits of n. *)
    if k >= n then k/2 else aux @@ 2*k
  in
  let k = aux 1 in
  2*(n - k) + 1

(* A334473. *)
let elephant2 n =
  let rec aux k = (* highest power of 3 in n. *)
    if k >= n then k/3 else aux @@ 3*k
  in
  let k = aux 1 in
  let b = n - k in
  if n <= 2 * k then b else 2*b - k

let () =
  (* PART 1 *)
  elephant elfs |> Printf.printf "%d\n";

  (* PART 2 *)
  elephant2 elfs |> Printf.printf "%d\n"
