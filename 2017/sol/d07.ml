open Ut

module S = Set.Make(String)

let parse str =
  let parse_children str =
    String.split_on_char ' ' str
    |> List.tl
    |> List.map (fun s ->
        String.to_seq s |> Seq.filter ((<>) ',') |> String.of_seq)
  in
  Scanf.sscanf str "%s (%d) %s@!" (fun name weight children ->
      if children = ""
      then name, (weight, [])
      else name, (weight, parse_children children))

let find_root graph =
  let everyone = List.map fst graph |> S.of_list
  and children = List.map (fun (_, (_, s)) -> S.of_list s) graph
                |> List.fold_left S.union S.empty
  in
  S.diff everyone children |> S.choose

let find_weight graph root =
  let rec aux node : (int * int, int) Either.t = (* my, total weight or weight need to be *)
    let weight, children = List.assoc node graph in
    let rec check ws = function
      | [] ->
        if ws = [] then Either.Left (weight, weight)
        else
          let fw = List.hd ws |> snd in
          let l, r = List.partition (fun (_, w) -> w = fw) ws in
          if r = [] then (* all same *)
            Left (weight, weight + (List.fold_left Int.add 0 @@ List.map snd l))
          else
            let o, m = if List.length l = 1 then l, r else r, l in
            let ow, ot = List.hd o and _, mt = List.hd m in
            Right (mt - (ot - ow))

      | c :: cs -> begin match aux c with
          | Left w -> check (w::ws) cs
          | Right _ as r -> r
        end
    in
    check [] children
  in
  match aux root with
  | Left _ -> assert false
  | Right n -> n

let () =
  let data = IO.read_lines () |> List.map parse in
  let root = find_root data in
  (* PART 1 *)
  root |> Printf.printf "%s\n";
  (* PART 2 *)
  find_weight data root |> Printf.printf "%d\n"
