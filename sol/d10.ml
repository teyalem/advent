open Ut

let to_digit c =
  Char.(code c - code '0')

let next n =
  let open List in
  let h = hd n and l = tl n in
  fold_left (fun (pn, c, ps) i ->
      if i = pn
      then i, c+1, ps
      else i, 1, (pn, c)::ps)
    (h, 1, [])
    l
  |> (fun (h, c, l) -> (h, c)::l)
  |> rev
  |> concat_map (fun (h, i) -> [i; h])

let repeat n f : 'a -> 'a =
  let rec aux i x =
    if i = n then x else aux (i+1) (f x)
  in
  aux 0

let () =
  let data =
    "1321131112"
    |> String.to_seq
    |> List.of_seq
    |> List.map to_digit
  in
  let solve n =
    repeat n next data |> List.length |> Printf.printf "%d\n";
  in
  begin (* SLOW *)
    (* PART 1 *) solve 40;
    (* PART 2 *) solve 50;
  end
