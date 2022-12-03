let rec split_data b = function
  | [] -> [b]
  | ""::ss -> b :: split_data [] ss
  | x::ss -> split_data (x::b) ss

let solve data =
  let open List in
  split_data [] data
  |> map (map int_of_string)
  |> map (fold_left (+) 0)
  |> sort (Fun.flip Int.compare)

let () =
  let d = open_in Sys.argv.(1) |> In_channel.input_all |> String.split_on_char '\n' in
  match solve d with
  | a :: b :: c :: _ -> Printf.printf "%d\n%d\n" a (a+b+c)
  | _ -> assert false
