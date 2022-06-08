let unit_type = Char.lowercase_ascii

let opposite a b =
  unit_type a = unit_type b
  && abs Char.(code a - code b) = 32 (* Am I right? *)

let full_react cs =
  List.fold_left (function
      | [] -> fun c -> [c]
      | p::ps -> fun c -> if opposite p c then ps else c::p::ps)
    []
    cs
  |> List.rev

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
