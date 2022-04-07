open Ut

let simulate n fish =
  let arr =
    List.fold_left
      (fun arr n -> arr.(n) <- arr.(n) + 1; arr)
      (Array.make 9 0)
      fish
  in
  let rec aux i arr =
    if i = n then arr
    else
      aux (i+1)
      @@ Array.init 9 (fun i ->
          if i = 6 then arr.(0) + arr.(7) (* timer reset + downtick *)
          else if i = 8 then arr.(0) (* new fish *)
          else arr.(i+1)) (* downtick *)
  in
  aux 0 arr
  |> Array.fold_left Int.add 0

let () =
  let data =
    IO.read_all ()
    |> String.split_on_char ','
    |> List.map int_of_string
  in
  begin
    (* PART 1 *)
    simulate 80 data |> Printf.printf "%d\n";

    (* PART 2 *)
    simulate 256 data |> Printf.printf "%d\n";
  end
