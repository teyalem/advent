open Ut

let is_digit c =
  '0' <= c && c <= '9'

let number seq =
  let n = Seq.take_while is_digit seq |> String.of_seq
  and rest = Seq.drop_while is_digit seq in
  int_of_string n, rest

module Packet = struct
  type t =
    | Num of int
    | List of t list

  let parse str =
    let rec aux buf seq =
      match seq () with
      | Seq.Nil -> List (List.rev buf), Seq.empty
      | Seq.Cons (c, rest) ->
        if c = '[' then
          let l, seq = aux [] rest in
          aux (l::buf) seq
        else if c = ']' then
          List (List.rev buf), rest
        else if is_digit c then
          let n, rest = number @@ Seq.cons c rest in
          aux (Num n :: buf) rest
        else if c = ',' then
          aux buf rest
        else
          assert false
    in
    match aux [] @@ String.to_seq str with
    | List [l], _ -> l
    | _ -> assert false

  let parse_pair str =
    match String.split_on_char '\n' @@ String.trim str with
    | [a; b] -> parse a, parse b
    | _ -> assert false

  let rec compare l r =
    match l, r with
    | Num a, Num b -> Int.compare a b
    | List a, List b -> compare_list a b

    | List a, Num b -> compare_list a [Num b]
    | Num a, List b -> compare_list [Num a] b

  and compare_list l r =
    match l, r with
    | [], [] -> 0
    | [], _b -> -1
    | _a, [] -> 1
    | a::al, b::bl ->
      let c = compare a b in
      if c = 0
      then compare_list al bl
      else c
end

let () =
  let data =
    open_in Sys.argv.(1)
    |> IO.input_all
    |> Str.(split @@ regexp "\n\n")
    |> List.map Packet.parse_pair
  in
  (* PART 1 *)
  data
  |> List.map (fun (a, b) -> Packet.compare a b < 0)
  |> List.mapi (fun i b -> if b then i+1 else 0)
  |> List.filter (fun i -> i > 0)
  |> List.fold_left (+) 0
  |> print_int;
  print_newline ();

  (* PART 2 *)
  let packets = data |> List.map (fun (a, b) -> [a; b]) |> List.concat in
  let a = Packet.parse "[[2]]"
  and b = Packet.parse "[[6]]" in
  let f n =
    List.filter (fun p -> Packet.compare p n < 0) packets
    |> List.length
  in
  print_int @@ (f a + 1) * (f b + 2)
