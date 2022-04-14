open Ut

let parse str =
  Scanf.sscanf str "Disc #%d has %d positions; at time=0, it is at position %d."
    (fun i n p -> n, (2*n - i - p) mod n)

let solve xs =
  let rec aux m i = function
    | [] -> m
    | (n, p) :: xs ->
      if m mod n = p
      then aux m (i*n) xs
      else aux (m+i) i ((n, p) :: xs)
  in
  match xs with
  | [] -> 0
  | (n, p) :: xs ->
    aux p n xs

let () =
  let data = IO.read_lines () |> List.map parse in
  begin
    (* PART 1 *)
    solve data |> Printf.printf "%d\n";

    (* PART 2 *)
    data @ [ (11, 11 - 1 - List.length data) ]
    |> solve
    |> Printf.printf "%d\n";
  end
