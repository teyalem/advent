let apply (arr, pos, skip_size) len =
  let alen = Array.length arr in
  let arr = Array.copy arr in
  Array.init len (fun i -> arr.((i + pos) mod alen))
  |> Array.iteri (fun i n -> arr.((pos + len - i - 1) mod alen) <- n);
  arr, pos + len + skip_size, skip_size + 1

let addtional_lengths = List.to_seq [ 17; 31; 73; 47; 23 ]

let to_lengths str =
  Seq.append
    (String.to_seq str |> Seq.map Char.code)
    addtional_lengths

let dense_hash sparse =
  Array.init 16 (fun i ->
      List.init 16 (fun j -> sparse.(16 * i + j))
      |> List.fold_left (lxor) 0)

let knot_hash data =
  let arr = Array.init 256 Fun.id in
  let lengths = to_lengths data in
  lengths
  |> Seq.cycle
  |> Seq.(take @@ 64 * length lengths)
  |> Seq.fold_left apply (arr, 0, 0)
  |> (fun (arr, _, _) -> arr)
  |> dense_hash

let hex arr =
  let b = Buffer.create 64 in
  Array.iter (Printf.bprintf b "%02x") arr;
  Buffer.contents b
