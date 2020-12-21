(*
 * Common patterns for AoC
 *)

(* Useful IO Operations *)
module IO = Io

(* inclusive range *)
module Range = struct
  type t = int * int

  let make s e = s, e
  let contains (s, e) n = s <= n && n <= e

end

type range = Range.t

(* DELIMITER OPERATIONS *)
module Delim = struct

  let split pat str = Str.(split (regexp pat) str)
  let split_line str = Str.(split (regexp "\n") str)

end
