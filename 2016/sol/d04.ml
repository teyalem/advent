open Ut

let parse str =
  Scanf.sscanf str "%s@[%s@]" (fun s ck ->
      match String.split_on_char '-' s |> List.rev with
      | id::rest ->
        List.rev rest, int_of_string id, ck
      | _ -> assert false)

(* pipeline mess *)
let gen_cksum str =
  String.to_seq str
  |> List.of_seq
  |> List.sort Char.compare (* sort chars for grouping *)
  |> List.to_seq
  |> Seq.group (=) (* group chars for counting *)
  |> Seq.map (fun cs -> (* char, count *)
      let c, _ = Option.get @@ Seq.uncons cs in
      c, Seq.length cs)
  |> List.of_seq (* sort alphabetically, then by commonness *)
  |> List.sort (fun (a, _) (b, _) -> Char.compare a b)
  |> List.sort (fun (_, a) (_, b) -> Int.compare b a)
  |> List.to_seq
  |> Seq.map fst (* need only chars *)
  |> Seq.take 5 (* take 5 most common letters *)
  |> String.of_seq

let check_sum (name, _, ck) =
  (String.concat "" name |> gen_cksum) = ck

let rotate n c =
  let k = Char.(code c - code 'a') in
  Char.(chr @@ (k + n) mod 26 + code 'a')

let decipher (name, id, _) =
  let name_dec =
    List.map (String.map @@ rotate id) name
    |> String.concat " "
  in
  name_dec, id

let () =
  let data = IO.read_lines () |> List.map parse in
  begin
    (* PART 1 *)
    List.filter check_sum data
    |> List.map (fun (_, id, _) -> id)
    |> List.fold_left (+) 0
    |> Printf.printf "%d\n";

    (* PART 2 *)
    List.map decipher data
    |> List.find (fun (name, _) -> name = "northpole object storage")
    |> snd
    |> Printf.printf "%d\n";
  end
