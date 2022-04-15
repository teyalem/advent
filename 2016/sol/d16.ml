open Ut

let curve seq =
  let f seq =
    List.of_seq seq
    |> List.rev_map (function
        | 0 -> 1
        | 1 -> 0
        | _ -> assert false)
    |> List.to_seq
  in
  List.to_seq [ seq; Seq.return 0; f seq ]
  |> Seq.concat

let rec expand len seq =
  if Seq.length seq >= len then seq
  else expand len @@ curve seq

let rec checksum ns =
  let rec aux l = function
    | a :: b :: xs -> aux ((if a = b then 1 else 0) :: l) xs
    | _ -> l
  in
  let c = aux [] ns |> List.rev in
  if List.length c mod 2 = 0 then checksum c else c

let initial_state =
  "01111001100111011"
  |> String.to_seq
  |> Seq.map (fun c -> Char.(code c - code '0'))
  |> Seq.memoize

let len1 = 272

(* this takes a while and made stack overflow *)
let len2 = 35651584

let solve len =
  expand len initial_state
  |> Seq.take len
  |> List.of_seq
  |> checksum
  |> List.iter (Printf.printf "%d");
  Printf.printf "\n"

let () =
  begin
    (* PART 1 *) solve len1;
    (* PART 2 *) solve len2;
  end
