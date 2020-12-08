let rec read_lines_from (file: in_channel) : string list =
  match input_line file with
  | line -> line::(read_lines_from file)
  | exception End_of_file -> []

let read_file (file: in_channel) : string =
  really_input_string file (in_channel_length file)

type path = { path: string }

let path s = { path = s }

let read_file_from_path path =
  let file = open_in path.path in 
  read_file file

let split pat str = Str.(split (regexp pat) str)

let split_line str = Str.(split (regexp "\n") str)
