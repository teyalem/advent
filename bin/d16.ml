open Advent

module Rule = struct
  open Range
  type t = range * range

  let rule r1 r2 = r1, r2

  let check num (r1, r2) =
    contains r1 num || contains r2 num

  (* parse a rule *)
  let parse str =
    Scanf.sscanf str "%s@: %d-%d or %d-%d"
      (fun name s1 e1 s2 e2 -> name, rule (s1, e1) (s2, e2))

  let is_matching_field rule fnum tickets =
    List.map (fun l -> List.nth l fnum) tickets
    |> List.for_all (fun n -> check n rule)

end

module Rules = struct
  type rule = Rule.t
  type t = (string * rule) list

  let parse str = Delim.split_line str |> List.map Rule.parse

  let check n rules = List.for_all (fun (_, r) -> Rule.check n r) rules

  let is_all_invalid n rules = List.exists (fun (_, r) -> Rule.check n r) rules |> not

  let get_rule name rules = List.assoc name rules

  (* returns name, field number list *)
  (* FIXME: too dirty *)
  let find_fields rules tickets = 
    let len = List.(length (nth tickets 0)) in
    let is_matched (name, rule) =
      let fields =
        List.(init len (fun i -> i)
              |> filter (fun i -> Rule.is_matching_field rule i tickets))
      in
      name, fields
    in
    let matching_fields =
      List.map is_matched rules
      (* sort by length *)
    |> List.sort (fun (_, l1) (_, l2) ->
          compare (List.length l1) (List.length l2))
    in

    (* remove an element from list *)
    let remove n = List.filter (fun m -> n <> m) in

    let rec loop = function
      | [] -> []
      | (name, fields) :: rest ->
        let field = List.nth fields 0 in
        let rest = List.map (fun (name, fl) -> name, remove field fl) rest in
        (name, field) :: (loop rest)
    in

    loop matching_fields

end

module Ticket = struct
  type t = int list

  let is_valid rules t =
    List.exists (fun n -> Rules.is_all_invalid n rules) t |> not

  let parse str =
    Delim.split "," str
    |> List.map int_of_string

  let print t =
    List.iter (Printf.printf " %d,") t;
    print_newline ()

end

(* Parsers *)

let parse_my_ticket str =
  match Delim.split_line str with
  | ["your ticket:"; t] -> Ticket.parse t
  | _ -> assert false

let parse_tickets str =
  match Delim.split_line str with
  | "nearby tickets:" :: ts -> List.map Ticket.parse ts
  | _ -> assert false

let parse_note str =
  let rules, my_t, nearby_ts =
    match Delim.split "\n\n" str with
    | [a; b; c] -> a, b, c
    | _ -> assert false
  in
  ( Rules.parse rules,
    parse_my_ticket my_t,
    parse_tickets nearby_ts)

let find_invalid_nums rules ticket =
  List.filter (fun num -> Rules.is_all_invalid num rules) ticket

let main path =
  let rules, my_t, nearby_ts =
    open_in path |> IO.read_file |> parse_note
  in
  begin
    (* PART 1 *)
    List.map (find_invalid_nums rules) nearby_ts
    |> List.filter_map (
      function [] -> None
             | [n] -> Some n
             | _ -> assert false
    )
    |> List.fold_left Int.add 0
    |> print_int;

    print_newline ();

    (* PART 2 *)
    let filtered_ts = List.filter (Ticket.is_valid rules) nearby_ts in
    let fields = Rules.find_fields rules (my_t::filtered_ts) in

    print_endline "Searching done"; (* Just for coolness :) *)

    let open List in
    (* get fields *)
    let dloc = assoc "departure location" fields |> nth my_t in
    let dsta = assoc "departure station" fields |> nth my_t in
    let dpla = assoc "departure platform" fields |> nth my_t in
    let dtra = assoc "departure track" fields |> nth my_t in
    let ddat = assoc "departure date" fields |> nth my_t in
    let dtim = assoc "departure time" fields |> nth my_t in

    print_int (dloc*dsta*dpla*dtra*ddat*dtim)

  end

let _ = Arg.parse [] main ""
