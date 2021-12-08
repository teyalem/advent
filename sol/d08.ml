(* crappy code *)

open Ut
open List

let unique_numbers = [ 2; 3; 4; 7; ]

let segment = [
  [ 'a'; 'b'; 'c'; 'e'; 'f'; 'g' ];
  [ 'c'; 'f' ];
  [ 'a'; 'c'; 'd'; 'e'; 'g'];
  [ 'a'; 'c'; 'd'; 'f'; 'g'];
  [ 'b'; 'c'; 'd'; 'f'];
  [ 'a'; 'b'; 'd'; 'f'; 'g'];
  [ 'a'; 'b'; 'd'; 'e'; 'f'; 'g'];
  [ 'a'; 'c'; 'f'];
  [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'];
  [ 'a'; 'b'; 'c'; 'd'; 'f'; 'g']
]

let all = [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g']

let rec permut = function
  | [] -> []
  | [x] -> [[x]]
  | l -> fold_left (fun acc x ->
      acc @ map (fun e -> x::e) (permut @@ filter ((<>) x) l))
      [] l

let segp = permut all |> map (map2 (fun a b -> b, a) all)

let to_num =
  fold_left (fun p n -> p * 10 + n) 0

let to_digit segs =
  try mapi (fun i s -> s, i) segment |> assoc segs
  with _ -> -1

let replace m seg =
  map (fun x -> assoc x m) seg
  |> sort Char.compare

let comp = List.compare Char.compare
let ints = List.init 10 Fun.id
let deduce segs =
  find
    (fun p ->
       let s = map (replace p) segs |> map to_digit |> sort Int.compare in
       s = ints)
    segp

let to_list s = String.to_seq s |> of_seq

let parse str : char list list * char list list =
  String.split_on_char '|' str
  |> map String.trim
  |> map (String.split_on_char ' ')
  |> map (map to_list)
  |> (fun l -> nth l 0, nth l 1)

let () =
  let data = open_in Sys.argv.(1)
             |> IO.input_lines
             |> map parse
  in
  begin
    (* PART 1 *)
    map snd data
    |> map (fun ss ->
        filter (fun s -> mem (length s) unique_numbers) ss
        |> length)
    |> sum
    |> Printf.printf "%d\n";

    (* PART 2 *)
    data
    |> map (fun (segs, outputs) ->
        let m = deduce segs in
        map (replace m) outputs
        |> map to_digit
        |> to_num)
    |> sum
    |> Printf.printf "%d\n"
  end
