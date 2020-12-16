(* UTILITY MODULES
 *)

(* for file reading *)
(* TODO: rename *)
include File

(* inclusive range *)
module Range = struct
  type t = int * int

  let make s e = s, e
  let contains (s, e) n = s <= n && n <= e

end

(*
 * STRING OPERATIONS
 *)

let split pat str = Str.(split (regexp pat) str)

let split_line str = Str.(split (regexp "\n") str)
