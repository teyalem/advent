open Advent

type colorcode = string * string (* indentifier for bags *)
type contents = (int * colorcode) list
type rule = colorcode * contents (* required contents for each bag *)
(* assume there's only one rule for each bag *)

let colorcode a b = a, b (* colorcode abstraction *)

(* RULE = Colorcode "bags" "contain" { Number Colorcode "bag"["s"]"," } Number
 * Colorcode "bag"["s"]"." *)
(* "no other bags" is special case and should be treated as []. *)
let rule code contains = code, contains

(* Parser for rules (Very Dirty) *)

exception Parse_fail

let is_code = function
  | "bags" | "bag" | "contain" -> false
  | _ -> true

let comsume s = function
  | h::xs when h = s -> xs
  | xs -> xs

let skip = function
  | _::ss -> ss
  | _ -> raise Parse_fail

let parse_code = function
  | a::b::xs ->
    if is_code a && is_code b then (colorcode a b), xs
    else raise Parse_fail
  | _ -> raise Parse_fail

let parse_num_opt = function
  | n::xs ->
    (match int_of_string n with
    | n -> (Some n), xs
    | exception Failure _ -> None, n::xs)
  | _ -> raise Parse_fail

let rec parse_contents ss =
  let num, ss = parse_num_opt ss in
  let code, ss = parse_code ss in
  if code = (colorcode "no" "other") then []
  else
    let ss = skip ss in
    match ss with
    | [] -> [Option.get num, code]
    | _ -> (Option.get num, code)::(parse_contents ss)

let parse_rule (str: string) : rule =
  let ss = Delim.split "[ .,]+" str in
  let code, ss = parse_code ss in
  let ss = skip ss in
  let ss = comsume "contain" ss in
  let bags = parse_contents ss in
  rule code bags

(* check the bag contains content *)
let rec contains (rules: rule list)  (content: colorcode) (bag: colorcode) : bool =
  let _, inner_bags = List.(assoc bag rules |> split)
  in if inner_bags = [] then false
  else if List.mem content inner_bags then true
  else List.exists (contains rules content) inner_bags

(* count total number of bags inside given bag*)
let rec count_inner_bags (rules: rule list) (bag: colorcode) : int =
  let inner_bags_with_num = List.assoc bag rules
  in if inner_bags_with_num = [] then 0
  else inner_bags_with_num
       |> List.fold_left (fun p (num, bag) -> p + num * (1 + (count_inner_bags rules bag))) 0

let main path =
  let rules = open_in path |> IO.read_lines |> List.map parse_rule
  in
  begin
    (* PART 1 *)
    count_inner_bags rules (colorcode "shiny" "gold")
    |> print_int;

    print_newline ();

    (* PART 2 *)
    rules
   |> List.map fst
   |> List.filter (contains rules (colorcode "shiny" "gold"))
   |> List.length
   |> print_int

 end
  
let _ = Arg.parse [] main ""
