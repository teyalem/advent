open Ut

module IP = struct
  type t = int * int
  let compare (a, b) (c, d) =
    match Int.compare a c with
    | 0 -> Int.compare b d
    | v -> v
end

let parse str =
  Scanf.sscanf str "%d-%d" (fun a b -> a, b)

let find_least_missing xs =
  let rec aux cg = function
    | [] -> assert false
    | (a, b) :: xs ->
      if a > cg+1 then cg+1 else aux (max cg b) xs
  in
  aux 0 xs

let n_allowed_ips maxn xs =
  let rec aux acc cg = function
    | [] -> acc + maxn - cg
    | (a, b) :: xs ->
      if a > cg+1
      then aux (acc + a - cg - 1) b xs
      else aux acc (max cg b) xs
  in
  aux 0 0 xs

let () =
  let data = IO.read_lines () |> List.map parse |> List.sort IP.compare in
  (* PART 1 *)
  data |> find_least_missing |> Printf.printf "%d\n";

  (* PART 2 *)
  data |> n_allowed_ips (1 lsl 32 - 1) |> Printf.printf "%d\n"
