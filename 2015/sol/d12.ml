open Ut

let is_num c =
  '0' <= c && c <= '9' || c = '-'

let is_same_class a b =
  is_num a = is_num b

let collect_numbers seq =
  let open Seq in
  group is_same_class seq
  |> filter (exists is_num)
  |> map (fun ns -> String.of_seq ns |> int_of_string)

let collect_nonred_numbers str =
  let open Yojson.Basic in
  let data = from_string str in
  let rec aux : t -> int list = function
    | `Assoc xs ->
      if List.exists (fun (_, v) -> v = `String "red") xs then []
      else
        List.map snd xs
        |> List.concat_map (function `Int n -> [n] | v -> aux v)
    | `List xs -> List.concat_map aux xs
    | `Int n -> [n]
    | _ -> []
  in
  aux data

let () =
  let data = IO.read_all () in
  begin
    (* PART 1 *)
    String.to_seq data
    |> collect_numbers
    |> Seq.fold_left (+) 0
    |> Printf.printf "%d\n";

    (* PART 2 *)
    collect_nonred_numbers data
    |> List.fold_left (+) 0
    |> Printf.printf "%d\n";
  end
