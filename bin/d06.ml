(* useful abstraction *)
type marks = string
type answers = char list

let contains ch s = String.contains s ch

let all_marked = List.init 26 (fun i -> Char.(chr ((code 'a') + i)))

let collect_answers (marks_list: marks list) : answers =
  let f ch = List.for_all (contains ch) marks_list
  in List.filter f all_marked

let read_marks_from (path: Util.path) : marks list list =
  let data = Util.read_file_from_path path in
  Util.split "\n\n" data |> List.map Util.split_line

let main path =
  read_marks_from (Util.path path)
  |> List.map collect_answers
  |> List.map List.length
  |> List.fold_left Int.add 0
  |> print_int

let _ = Arg.parse [] main ""
