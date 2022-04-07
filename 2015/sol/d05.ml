open Ut

let nvowels str =
  let is_vowel = function
    | 'a' | 'e' | 'i' | 'o' | 'u' -> true
    | _ -> false
  in
  Seq.fold_left (fun p c -> p + if is_vowel c then 1 else 0) 0 str

let double str =
  let rec aux p seq =
    match seq () with
    | Seq.Nil -> false
    | Seq.Cons (c, seq) ->
      if p = c then true else aux c seq
  in
  match str () with
  | Seq.Nil -> false
  | Seq.Cons (c, seq) -> aux c seq

let exists_naughty str =
  Seq.windows 2 str
  |> Seq.exists (fun s ->
      let s = String.of_seq s in
      s = "ab" || s = "cd" || s = "pq" || s = "xy")

let is_nice_part1 str =
  nvowels str >= 3 && double str && not (exists_naughty str)

let pair_twice str =
  let rec aux = function
    | [] | [_] -> false
    | x :: y :: xs ->
      if List.mem x xs then true
      else aux (y::xs)
  in
  Seq.windows 2 str
  |> Seq.map String.of_seq
  |> List.of_seq
  |> aux

let sandwich str =
  Seq.windows 3 str
  |> Seq.exists (fun s ->
      match List.of_seq s with
      | [ a; _; b ] -> a = b
      | _ -> assert false)

let is_nice_part2 str =
  pair_twice str && sandwich str

let () =
  let data = IO.read_lines () |> List.map String.to_seq in
  let f rule =
    List.filter rule data
    |> List.length
    |> Printf.printf "%d\n"
  in
  begin
    (* PART 1 *) f is_nice_part1;
    (* PART 2 *) f is_nice_part2;
  end
