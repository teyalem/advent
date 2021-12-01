open Ut

let count_incr seq =
  Seq.fold_left
    (fun (c, pv) n -> (c + if n > pv then 1 else 0), n)
    (0, Int.max_int)
    seq
  |> fst

let () =
  let data =
    open_in Sys.argv.(1) |> IO.input_lines
    |> List.map int_of_string
    |> List.to_seq
  in
  begin
    (* part 1 *)
    count_incr data
    |> Printf.printf "%d\n";

    (* part 2 *)
    Useq.windows 3 data
    |> Seq.map (Seq.fold_left Int.add 0)
    |> count_incr
    |> Printf.printf "%d\n"
  end
