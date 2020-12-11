let preamable_n = 25 (* numbers of preamables *)

(* split lists into two lists, length of first being n. *)
let rec split n = function
  | [] -> if n = 0 then [], [] else assert false
  | x::xs ->
    if n = 0 then [], xs
    else
      let a, b = split (n-1) xs in
      (x::a), b

(* take first n elements from list. *)
let rec take n = function
  | [] -> if n = 0 then [] else assert false
  | x::xs -> if n = 0 then [] else x::(take (n-1) xs)

let push n = function
  | [] -> assert false
  | _::xs -> xs@[n]

(* check it's valid number.
 * that is, if there are two numbers that adds up to the number, it's valid.
 *)
let rec is_valid number = function
  | [] -> false
  | a::ns ->
    List.exists (fun b -> a + b = number) ns
    || is_valid number ns

(* find first invalid number from stream. *)
let rec find_first_invalid_opt prev = function
  | [] -> None
  | number::rest ->
    if is_valid number prev then
      let prev = push number prev in
      find_first_invalid_opt prev rest
    else Some number

(* find encryption weakness.
 * for i = 0 to length do
 *   let subl = l[i..length]
 *   let try = add l until it exceeds first invalid number
 *   if try = first_invalid then Some (i, j)
 *   else continue
 * done
 *)
let rec find_weakness finum stream = 
  let rec find_end_sum_until limit total = function
    | [] -> None
    | n::ns ->
      let total = total + n
      in if total > limit then None
      else if total = limit then Some n
      else find_end_sum_until limit total ns
  in
  match find_end_sum_until finum 0 stream with
  | None -> find_weakness finum (List.tl stream)
  | Some e -> (List.hd stream), e

let find_list s e l =
  List.filter (fun n -> s <= n) l
  |> List.filter (fun n -> n <= e)

let main path =
  let numbers = Util.read_lines_from (open_in path)
              |> List.map (int_of_string)
  in
  begin
    let prev, stream = split preamable_n numbers in
    let finum = find_first_invalid_opt prev stream |> Option.get in
    print_int finum;
    print_newline ();
    let (st, en) = find_weakness finum numbers in
    let l = find_list st en numbers in
    let max = List.(fold_left max (hd l) (tl l))
    and min = List.(fold_left min (hd l) (tl l))
    in Printf.printf "%d\n" (max + min)
  end

let _ = Arg.parse [] main ""
