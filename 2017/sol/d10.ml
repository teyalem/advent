open Ut

let print_arr arr =
  Array.iter (Printf.printf " %d") arr;
  Printf.printf "\n"

let apply (arr, pos, skip_size) len =
  let alen = Array.length arr in
  let arr = Array.copy arr in
  Array.init len (fun i -> arr.((i + pos) mod alen))
  |> Array.iteri (fun i n -> arr.((pos + len - i - 1) mod alen) <- n);
  arr, pos + len + skip_size, skip_size + 1

let validation_number (arr, _, _) = arr.(0) * arr.(1)

let addtional_lengths = List.to_seq [ 17; 31; 73; 47; 23 ]

let to_lengths str =
  Seq.append
    (String.to_seq str |> Seq.map Char.code)
    addtional_lengths

let dense_hash sparse =
  Array.init 16 (fun i ->
      List.init 16 (fun j -> sparse.(16 * i + j))
      |> List.fold_left (lxor) 0)

let print_hash h =
  Array.iter (Printf.printf "%02x") h;
  Printf.printf "\n"

let () =
  let data = IO.read_all () |> String.trim in
  (* PART 1 *)
  let lengths =
    data
    |> String.split_on_char ','
    |> List.map String.trim
    |> List.map int_of_string
  in
  let arr = Array.init 256 Fun.id in
  List.fold_left apply (arr, 0, 0) lengths
  |> validation_number
  |> Printf.printf "%d\n";

  (* PART 2 *)
  let arr = Array.init 256 Fun.id in
  let lengths = to_lengths data in
  lengths
  |> Seq.cycle
  |> Seq.(take @@ 64 * length lengths)
  |> Seq.fold_left apply (arr, 0, 0)
  |> (fun (arr, _, _) -> arr)
  |> dense_hash
  |> print_hash;
