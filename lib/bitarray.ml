(* array of bits *)

(* bits. 0 is lst and len - 1 is mst. *)
type t = int array

let get bs i = bs.(i)
let set bs i n = bs.(i) <- n

let of_int n =
  let rec inner n =
    if n = 0 then assert false
    else if n = 1 then [1]
    else
      (n mod 2) :: (inner (n/2))
  in
  let bits = inner n |> Array.of_list in
  Array.init 36
    (fun i -> match bits.(i) with
       | b -> b
       | exception Invalid_argument _ -> 0)

let to_int bs =
  Array.fold_right (fun n p -> p * 2 + n) bs 0
