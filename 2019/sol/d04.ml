open Ut

let to_digitlist n =
  Seq.unfold
    (fun n ->
      if n = 0
      then None
      else Some (n mod 10, n / 10))
    n
  |> List.of_seq
  |> List.rev

let test_two f l =
  let rec loop p = function
    | [] -> false
    | n :: ns ->
      if f p n
      then true
      else loop n ns
  in
  List.(loop (hd l) (tl l))

let decrease dl =
  test_two (fun p n -> p > n) dl

let has_same_adj dl =
  test_two (fun p n -> p = n) dl

let has_two_adj dl =
  List.fold_left (fun l n ->
      let i, c = List.hd l in
      if n = i
      then (i, c+1) :: List.tl l
      else (n, 1) :: l)
    [ List.hd dl, 1 ]
    (List.tl dl)
  |> List.exists (fun (_, c) -> c = 2)

let count_passwords f s e =
  Range.make s e
  |> Range.to_seq
  |> Seq.map to_digitlist
  |> Seq.filter f
  |> List.of_seq
  |> List.length

let main _ =
  let s, e = 265275, 781584 in
  begin
    (* PART 1 *)
    let rule1 dl = has_same_adj dl && not @@ decrease dl in
    count_passwords rule1 s e
    |> print_int;

    print_newline ();

    (* PART 2 *)
    let rule2 dl = rule1 dl && has_two_adj dl in
    count_passwords rule2 s e
    |> print_int

  end

let () = main ""
