open Advent

(* useful abstraction *)
type marks = string
type answers = char list

let contains ch s = String.contains s ch

(* List that contains all markes *)
let all_marked = List.init 26 (fun i -> Char.(chr ((code 'a') + i)))

let collect_answers_1 marks_list =
  let f ch = List.exists (contains ch) marks_list
  in List.filter f all_marked

let collect_answers_2 marks_list =
  let f ch = List.for_all (contains ch) marks_list
  in List.filter f all_marked

let parse str =
  Delim.split "\n\n" str |> List.map Delim.split_line

let main path =
  let data = open_in path |> IO.read_file |> parse in
  begin
    (* PART 1 *)
    data
    |> List.map collect_answers_1
    |> List.map List.length
    |> sum |> print_int;

    print_newline ();

    (* PART 2 *)
    data
    |> List.map collect_answers_2
    |> List.map List.length
    |> sum |> print_int
  end

let _ = Arg.parse [] main ""
