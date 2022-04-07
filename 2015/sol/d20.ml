let number = 36_000_000

let first_over n arr =
  Array.to_seq arr
  |> Seq.mapi (fun i n -> i, n)
  |> Seq.find (fun (_, x) -> x >= n)
  |> Option.get
  |> fst

let find_number n =
  let arr = Array.make (n+1) 1 in
  let update i = (* can be changed to use seqs *)
    let j = ref i in
    while !j <= n do
      arr.(!j) <- arr.(!j) + i;
      j := !j + i;
    done
  in
  Seq.ints 2 |> Seq.take (n/2) |> Seq.iter update;
  first_over n arr

let find_number2 n =
  let arr = Array.make (n+1) 1 in
  let update i =
    for j = 1 to 50 do
      if j*i <= n then arr.(j*i) <- arr.(j*i) + i;
    done
  in
  Seq.ints 2 |> Seq.take (n/2) |> Seq.iter update;
  first_over n arr

let () =
  begin
    (* PART 1 *)
    find_number (number/10) |> Printf.printf "%d\n";

    (* PART 2 *)
    find_number2 (number/11) |> Printf.printf "%d\n";
  end
