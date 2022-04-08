open Ut

let parse str =
  str
  |> String.map (fun c -> if c = '[' || c = ']' then '|' else c)
  |> String.split_on_char '|'
  |> List.mapi (fun i s -> i mod 2, s)
  |> List.partition (fun (i, _) -> i = 0)
  |> (fun (a, b) -> List.(map snd a, map snd b))

let has_abba str =
  let rec aux = function
    | a::b::c::d::xs ->
      if a = d && b = c && a <> c
      then true
      else aux (b::c::d::xs)
    | _ -> false
  in
  String.to_seq str |> List.of_seq |> aux

let supports_tls (addr, hnet) =
  List.exists has_abba addr &&
  List.for_all (Fun.negate has_abba) hnet

let find_aba str =
  let rec aux = function
    | a::b::c::xs ->
      if a = c && a <> b
      then (a, b) :: aux (b::c::xs)
      else aux (b::c::xs)
    | _ -> []
  in
  String.to_seq str |> List.of_seq |> aux

let match_bab (a, b) str =
  let rec aux = function
    | x::y::z::xs ->
      if x = b && y = a && z = b then true else aux (y::z::xs)
    | _ -> false
  in
  String.to_seq str |> List.of_seq |> aux

let supports_ssl (addr, hnet) =
  let open List in
  concat_map find_aba addr
  |> exists (fun ab -> exists (match_bab ab) hnet)

let () =
  let data = IO.read_lines () |> List.map parse in
  begin
    (* PART 1 *)
    data
    |> List.filter supports_tls
    |> List.length
    |> Printf.printf "%d\n";

    (* PART 2 *)
    data
    |> List.filter supports_ssl
    |> List.length
    |> Printf.printf "%d\n";
  end
