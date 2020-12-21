open Advent
open Scanf

(* association list *)
type passport = (string * string) list

let of_string (str: string) : passport =
  let rec inner scanner =
    match scanner (fun a b -> a, b) with
    | "", "" -> []
    | key, value -> (key, value)::(inner scanner)
  in inner (sscanf str "%s@:%s ")

let read_passports str : passport list =
  Delim.split "\n\n" str
  |> List.map of_string

let is_valid_byr year =
  let year = int_of_string year in
  1920 <= year && year <= 2002

let is_valid_iyr year =
  let year = int_of_string year in
  2010 <= year && year <= 2020

let is_valid_eyr year =
  let year = int_of_string year in
  2020 <= year && year <= 2030

let is_valid_hgt hgt =
  sscanf (String.trim hgt) "%d%s" (fun n u ->
      match u with
      | "cm" -> 150 <= n && n <= 193
      | "in" -> 59 <= n && n <= 76
      | _ -> false)

let is_valid_hcl hcl =
  ((String.length hcl) = 7)
  && (Str.(string_match (regexp "#[0-9a-f]+") hcl 0))

let is_valid_ecl = function
  | "amb" | "blu" | "brn" 
  | "gry" | "grn" | "hzl" | "oth" -> true
  | _ -> false

let is_valid_pid pid = (String.length pid) = 9

let check_fun = function
  | "byr" -> is_valid_byr
  | "iyr" -> is_valid_iyr
  | "eyr" -> is_valid_eyr
  | "hgt" -> is_valid_hgt
  | "hcl" -> is_valid_hcl
  | "ecl" -> is_valid_ecl
  | "pid" -> is_valid_pid
  | "cid" -> fun _ -> true
  | _ -> assert false

let is_valid passport =
  match List.length passport with (* check field numbers first *)
  | 8 | 7 as len ->
    (* check optional field; The only optional field is "cid", so when
     * the length is 7 and "cid" is present, it is invalid passport!
     *)
    if len = 7 && (List.mem_assoc "cid" passport) then
      false
    else 
      List.(passport
          |> map (fun (key, value) -> (check_fun key) value)
          |> fold_left (&&) true)
  | _ -> false

let main path =
  let data = open_in path |> IO.read_file |> read_passports in
  begin

    (* PART 2 *)
    data
    |> List.filter is_valid
    |> List.length
    |> print_int

  end

let _ = Arg.parse [] main ""
