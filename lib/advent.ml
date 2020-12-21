(*
 * Common patterns for AoC
 *)

(* reverse array *)
let rev_array arr =
  Array.to_list arr
  |> List.rev
  |> Array.of_list

(* return sum of all elements in list l *)
let sum l = List.fold_left Int.add 0 l

(* starts_with pat str checks that string str starts with pattern pat. *)
let starts_with pat str =
  let pat = Str.regexp ("^" ^ pat) in
  Str.string_match pat str 0

module IO = Io (* Useful IO Operations *)
module Graph = Graph
module Block = Block
module Bitarray = Bitarray

(* inclusive range *)
module Range = struct
  type t = int * int

  let make s e = s, e
  let contains (s, e) n = s <= n && n <= e

end

(* DELIMITER OPERATIONS *)
module Delim = struct
  let split pat str = Str.(split (regexp pat) str)
  let split_line str = Str.(split (regexp "\n") str)
end

(* types *)
type graph = Graph.t
type range = Range.t
