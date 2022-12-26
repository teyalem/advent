open Ut

(* https://old.reddit.com/r/adventofcode/comments/zqezkn/2022_day_20_solutions/j0xxlsc/ *)
let mix ?(part2 = false) data =
  let rec insert i el = function
    | [] -> if i = 0 then [el] else assert false
    | x :: xs ->
      if i = 0 then el :: x :: xs
      else x :: (insert (i - 1) el xs)
  in

  let aux l (_, o as el) =
    let loc = ref 0 in
    List.iteri (fun j x -> if x = el then loc := j) l;
    let front, back =
      l
      |> List.mapi (fun i n -> i, n)
      |> List.partition (fun (i, _) -> i < !loc)
    in
    let nl = (front @ List.tl back) |> List.map snd in
    let len = List.length nl in
    let dst = (len + (!loc + o) mod len) mod len in
    insert dst el nl
  in
  let l = List.mapi (fun i n -> i, n) data in
  let t =
    if part2 then
      Seq.init 10 (fun _ -> List.to_seq l) |> Seq.concat
    else
      List.to_seq l
  in
  List.length data,
  (Seq.fold_left aux l t
  |> List.map snd
  |> Array.of_list)

let find_coord (len, arr) =
  let zero = ref 0 in
  Array.iteri (fun i n -> if n = 0 then zero := i) arr;
  [1000; 2000; 3000]
  |> List.map (fun n ->
      let i = (n + !zero) mod len in
      arr.(i))

let decryption_key = 811589153

let () =
  let data = open_in Sys.argv.(1) |> IO.input_lines |> List.map int_of_string in
  mix data
  |> find_coord
  |> List.fold_left (+) 0
  |> print_int;
  print_newline ();

  data
  |> List.map (fun n -> decryption_key * n)
  |> mix ~part2: true
  |> find_coord
  |> List.fold_left (+) 0
  |> print_int
