open Ut

let length = String.length

let nchars str =
  let str = String.to_seq str |> List.of_seq in
  let rec aux n = function
    | [] -> n
    | '"' :: xs -> aux n xs
    | '\\' :: 'x' :: _ :: _ :: xs
    | '\\' :: '\\' :: xs
    | '\\' :: '"' :: xs
    | _ :: xs -> aux (n+1) xs
  in
  aux 0 str

let nchars_encoded str =
  let str = String.to_seq str |> List.of_seq in
  let rec aux n = function
    | [] -> n
    | '"' :: xs -> aux (n+2) xs
    | '\\' :: '\\' :: xs
    | '\\' :: '"' :: xs -> aux (n+4) xs
    | '\\' :: 'x' :: _ :: _ :: xs -> aux (n+5) xs
    | _ :: xs -> aux (n+1) xs
  in
  aux 0 str + 2

let () =
  let data = IO.read_lines () in
  begin
    (* PART 1 *)
    List.map (fun s -> length s - nchars s) data
    |> sum
    |> Printf.printf "%d\n";

    (* PART 2 *)
    List.map (fun s -> nchars_encoded s - length s) data
    |> sum
    |> Printf.printf "%d\n";
  end
