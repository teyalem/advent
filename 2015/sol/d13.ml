open Ut

let parse str =
  Scanf.sscanf str "%s would %s %d happiness units by sitting next to %s@."
    (fun a sign w b ->
       (a, b), w *
       (if sign = "gain" then +1
       else if sign = "lose" then -1
       else assert false))

let permut xs =
  let len = List.length xs in
  let fn = Seq.init len succ |> Seq.fold_left Int.mul 1 in
  let rec select n len xs =
    if xs = [] then []
    else
      let nn, ni = n / len, n mod len in
      List.nth xs ni ::
      select nn (len - 1) (List.filteri (fun i _ -> i <> ni) xs)
  in
  Seq.unfold (fun i -> if i < fn then Some (select i len xs, i+1) else None) 0

let collect_names graph =
  List.map (fun ((n, _), _) -> n) graph
  |> List.sort_uniq String.compare

let calc_score scores table =
  let table = Array.of_list table in
  let len = Array.length table in
  let np = List.init len
      (fun i -> table.(i), table.((i+1) mod len))
  in
  np
  |> List.map (fun (a, b) ->
      List.assoc (a, b) scores +
      List.assoc (b, a) scores)
  |> List.fold_left (+) 0

let () =
  let data = IO.read_lines () |> List.map parse in
  let names = collect_names data in
  begin
    (* PART 1 *)
    let all = permut names in
    Seq.map (calc_score data) all
    |> Seq.fold_left max min_int
    |> Printf.printf "%d\n";

    (* PART 2 *)
    let data =
      List.concat_map (fun n -> [ ("Me", n), 0; (n, "Me"), 0 ]) names @
      data
    in
    let all = permut @@ "Me" :: names in
    Seq.map (calc_score data) all
    |> Seq.fold_left max min_int
    |> Printf.printf "%d\n";
  end
