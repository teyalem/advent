let unit_type = Char.lowercase_ascii

let opposite a b =
  unit_type a = unit_type b
  && abs Char.(code a - code b) = 32 (* Am I right? *)

let react cs =
  let rec aux = function
    | [] -> 0, []
    | [a] -> 0, [a]
    | a::b::cs ->
      if opposite a b then
        let cnt, cs = aux cs in
        cnt + 1, cs
      else
        let cnt, cs = aux (b::cs) in
        cnt, a::cs
  in
  aux cs

let full_react cs =
  let rec aux cs =
    let cnt, cs = react cs in
    if cnt = 0 then cs else aux cs
  in
  aux cs

let shortest_polylen cs =
  Seq.init 26 (fun i -> Char.(chr @@ code 'a' + i)) (* a..z *)
  |> Seq.map (fun c -> List.filter (fun d -> unit_type d <> c) cs)
  |> Seq.map full_react
  |> Seq.map List.length
  |> Seq.fold_left min max_int

let () = Printexc.record_backtrace true

let () =
  let data =
    open_in Sys.argv.(1)
    |> In_channel.input_all
    |> String.trim
    |> String.to_seq
    |> List.of_seq
  in
  (* PART 1 *)
  full_react data |> List.length |> print_int;
  print_newline ();

  (* PART 2 *)
  shortest_polylen data |> print_int;
