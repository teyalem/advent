let rec read_lines (file: in_channel) : string list =
  match input_line file with
  | line -> line::(read_lines file)
  | exception End_of_file -> []

let read_file (file: in_channel) : string =
  really_input_string file (in_channel_length file)
