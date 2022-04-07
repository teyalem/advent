open Ut

let parse str =
  Scanf.sscanf str " %d %d %d" (fun x y z -> [x; y; z])

let rec aux seq : 'a list Seq.t = fun () ->
  if Seq.is_empty seq then
    Seq.Nil
  else
    let l = Seq.take 3 seq |> List.of_seq |> List.sort Int.compare
    and seq = Seq.drop 3 seq in
    Seq.Cons (l, aux seq)

let transform ls =
  let ls = List.to_seq ls in
  let col i = Seq.map (fun l -> List.nth l i) ls in
  Seq.init 3 Fun.id
  |> Seq.concat_map col
  |> aux

let is_possible = function
  | [a; b; c] -> a+b > c
  | _ -> assert false

let () =
  let data = IO.read_lines () |> List.map parse in
  begin
    (* PART 1 *)
    data
    |> List.map (List.sort Int.compare)
    |> List.filter is_possible
    |> List.length
    |> Printf.printf "%d\n";

    (* PART 2 *)
    transform data
    |> Seq.filter is_possible
    |> Seq.length
    |> Printf.printf "%d\n";
  end
