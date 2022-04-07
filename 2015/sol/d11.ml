open Ut

let alpha_to_num c =
  Char.(code c - code 'a')

let encode str =
  String.to_seq str
  |> Seq.map alpha_to_num
  |> Array.of_seq

let decode arr =
  Array.to_seq arr
  |> Seq.map (fun c -> Char.(chr @@ c + code 'a'))
  |> String.of_seq

let a = alpha_to_num 'a'
let i = alpha_to_num 'i'
let o = alpha_to_num 'o'
let l = alpha_to_num 'l'
let z = alpha_to_num 'z'

let forbidden_letter c =
  c = i || c = o || c = l

let next arr =
  let arr = Array.copy arr in
  let rec aux i =
    if i >= 0 then begin
      arr.(i) <- arr.(i) + 1;
      if forbidden_letter arr.(i) then aux i;
      if arr.(i) > z then begin
        arr.(i) <- a;
        aux (i-1)
      end
    end
  in
  let last = Array.length arr - 1 in
  aux last; arr

let validate arr =
  let len = Array.length arr in
  let increasing_three i =
    arr.(i) + 1 = arr.(i+1) && arr.(i+1) + 1 = arr.(i+2)
  in
  let open List in
  let count_pair arr =
    Array.to_list arr
    |> filteri (fun i c -> i < len - 1 && arr.(i+1) = c)
    |> sort_uniq Int.compare
    |> length
  in
  let iota = init (len-2) Fun.id in
  exists increasing_three iota && count_pair arr >= 2

let solve d =
  let rec aux d =
    if validate d then d
    else aux @@ next d
  in
  aux d

let input = "hxbxwxba"

let () =
  let data = encode input in
  begin
    (* PART 1 *)
    let p1 = solve data in
    decode p1 |> Printf.printf "%s\n";

    (* PART 2 *)
    let p2 = solve @@ next p1 in
    decode p2 |> Printf.printf "%s\n";
  end
