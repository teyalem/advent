open Ut

let parse str =
  Scanf.sscanf str "%s@: %s@!" (fun name rest ->
      name,
      (match int_of_string_opt rest with
      | None -> Scanf.sscanf rest "%s %c %s" (fun l o r -> Either.Right (l, o, r))
      | Some n -> Left n))

let rec yell monkeys name =
  match List.assoc name monkeys with
  | Either.Left n -> n
  | Right (l, o, r) ->
    let ln = yell monkeys l and rn = yell monkeys r in
    (match o with
     | '+' -> ln + rn
     | '-' -> ln - rn
     | '*' -> ln * rn
     | '/' -> ln / rn
     | _ -> assert false)

let find_humn monkeys =
  let left, right =
    match List.assoc "root" monkeys with
    | Either.Right (l, _, r) -> l, r
    | _ -> assert false
  in
  let human = "humn" in

  let rec contains name root =
    if name = human then true
    else
      match List.assoc root monkeys with
      | Either.Right (l, _, r) -> contains name l || contains name r
      | _ -> false
  in

  let h, target =
    if contains human left
    then left, yell monkeys right
    else right, yell monkeys left
  in

  let low, high = (0, max_int/128) in
  let yell_with humn =
    let monkeys =
      List.map (fun (i, n) ->
          i, (if i = human then Either.Left humn else n))
        monkeys
    in
    yell monkeys h
  in
  let ln = yell_with low and hn = yell_with high in
  let rev = ln > hn in

  (* binary search *)
  let rec search (low, high) =
    if low > high then assert false
    else
      let humn = (low + high) / 2 in
      let n = yell_with humn in
      if n = target then humn
      else if rev then begin
        if n > target
        then search (humn + 1, high)
        else search (low, humn)
      end
      else begin
        if n < target
        then search (humn + 1, high)
        else search (low, humn)
      end
  in
  search (low, high)


let () =
  let data = open_in Sys.argv.(1) |> IO.input_lines |> List.map parse in
  yell data "root" |> print_int;
  print_newline ();

  find_humn data |> print_int
