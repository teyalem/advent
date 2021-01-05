open Advent

(* solved by hand:
 * space law space brochure
 * astrolabe
 * antenna
 * weather machine
 *)
let main path =
  let data = open_in path |> IO.read_file |> IntCode.parse_code in
  let m = IntCode.load data in
  IntCode.set_interactive m true;
  IntCode.run m

let () = Arg.parse [] main ""
