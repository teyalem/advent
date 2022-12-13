open Ut
open Printf

type t =
  | Num of int
  | List of t list

let is_digit c =
  '0' <= c && c <= '9'

let number seq =
  let n = Seq.take_while is_digit seq |> String.of_seq
  and rest = Seq.drop_while is_digit seq in
  int_of_string n, rest

let parse_list str =
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

let parse str =
  match String.split_on_char '\n' @@ String.trim str with
  | [a; b] -> parse_list a, parse_list b
  | _ -> assert false

let rec in_order l r =
  match l, r with
  | Num a, Num b ->
    if a = b then None else Some (a < b)
  | List a, List b ->
    (match a, b with
     | [], [] -> None
     | [], _ -> Some true
     | _, [] -> Some false
     | a::al, b::bl ->
       (match in_order a b with
        | None -> in_order (List al) (List bl)
        | Some _ as o -> o))

  | (List _ as a), (Num _ as b) -> in_order a (List [b])
  | (Num _ as a), (List _ as b) -> in_order (List [a]) b

let packet_lt a b =
  match in_order a b with
  | None -> failwith "Order cannot be determined"
  | Some b -> b

let compare_packet a b =
  if packet_lt a b then -1 else 1

let divider_packets = [
  "[[2]]";
  "[[6]]";
] |> List.map parse_list

let () = Printexc.record_backtrace true
let () =
  let data =
    open_in Sys.argv.(1)
    |> IO.input_all
    |> Str.(split @@ regexp "\n\n")
    |> List.map parse
  in
  printf "data parsed\n";
  data
  |> List.map (fun (a, b) -> packet_lt a b)
  |> List.mapi (fun i b -> if b then i+1 else 0)
  |> List.filter (fun i -> i > 0)
  |> List.fold_left (+) 0
  |> print_int;
  print_newline ();

  let data = data |> List.map (fun (a, b) -> [a; b]) |> List.concat in
  divider_packets @ data
  |> List.sort compare_packet
  |> List.mapi (fun i p -> i+1, p)
  |> List.filter_map (fun (i, p) ->
      if List.mem p divider_packets then Some i else None)
  |> List.fold_left Int.mul 1
  |> print_int
