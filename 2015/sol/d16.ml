open Ut

module M = Map.Make(String)

let parse_prop str =
  Scanf.sscanf str " %s@: %d" (fun s d -> s, d)

let parse str =
  let props str =
    String.split_on_char ',' str
    |> List.map parse_prop
  in
  Scanf.sscanf str "Sue %d: %s@!" (fun i p ->
      ("Sue", i) :: props p)
  |> List.to_seq
  |> M.of_seq

let needed =
  "children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1"
  |> String.split_on_char '\n'
  |> List.map parse_prop
  |> List.to_seq
  |> M.of_seq

let find_sue_part1 =
  List.find (fun m ->
      m |> M.for_all (fun k v ->
          let n = M.find_opt k needed in
          n = None || n = Some v))

let find_sue_part2 sues =
  let matches k v =
    let n = M.find_opt k needed in
    if Option.is_none n then true
    else
      let n = Option.get n in
      match k with
      | "cats" | "trees" -> v > n
      | "pomeranians" | "goldfish" -> v < n
      | _ -> v = n
  in
  List.find (fun m -> M.for_all matches m) sues

let () =
  let data = IO.read_lines () |> List.map parse in
  begin
    (* PART 1 *)
    find_sue_part1 data |> M.find "Sue" |> Printf.printf "%d\n";

    (* PART 2 *)
    find_sue_part2 data |> M.find "Sue" |> Printf.printf "%d\n";
  end
