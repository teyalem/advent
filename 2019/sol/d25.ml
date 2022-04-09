open Ut

(* solved by hand:
 * space law space brochure
 * astrolabe
 * antenna
 * weather machine
 *)
let () =
  let data = IO.read_all () |> IntCode.parse_code in
  let m = IntCode.load data in
  IntCode.set_interactive m true;
  IntCode.run m
