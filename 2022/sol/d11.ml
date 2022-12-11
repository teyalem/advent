open Ut

type monkey = {
  operation : string * char * string;
  test : int;
  iftrue : int;
  iffalse : int;
}

let list_of_string str =
  String.split_on_char ',' str
  |> List.map (fun n -> String.trim n |> int_of_string)

let parse str =
  Scanf.sscanf str "Monkey %d:
    Starting items: %s@\n Operation: new = %s %c %s
    Test: divisible by %d
        If true: throw to monkey %d
        If false: throw to monkey %d"
    (fun _monkey items o1 op o2 rem tm fm ->
       { operation = o1, op, o2;
         test = rem;
         iftrue = tm;
         iffalse = fm;
       },
       list_of_string items
    )

let turn part2 mn i monkeys monkeys_items =
  let monkey = monkeys.(i) in
  let items = monkeys_items.(i) in

  let inspect worry =
    let a, op, b = monkey.operation in
    let aux n = if n = "old" then worry else int_of_string n in
    let worry =
      match op with
      | '+' -> aux a + aux b
      | '*' -> aux a * aux b
      | _ -> assert false
    in
    if part2 then worry mod mn else worry / 3
  in

  let test item =
    let w = inspect item in
    (if w mod monkey.test = 0
    then monkey.iftrue
    else monkey.iffalse)
    , w
  in

  monkeys_items.(i) <- [];
  List.map test items
  |> List.iter (fun (j, w) -> monkeys_items.(j) <- monkeys_items.(j) @ [w])

let round part2 monkey_number monkeys monkeys_items =
  let len = Array.length monkeys in
  List.init len (fun i ->
      let n = List.length monkeys_items.(i) in
      turn part2 monkey_number i monkeys monkeys_items;
      n)

let monkey_business inspects =
  inspects
  |> List.sort (Fun.flip Int.compare)
  |> (function
      | a::b::_ -> a*b
      | _ -> assert false)

let monkey_number monkeys =
  Array.to_seq monkeys
  |> Seq.map (fun m -> m.test)
  |> Seq.fold_left Int.mul 1

let solve ?(part2 = false) monkeys monkeys_items =
  let rounds = if part2 then 10000 else 20 in
  let mn = monkey_number monkeys in
  match List.init rounds (fun _ -> round part2 mn monkeys monkeys_items) with
  | [] -> assert false
  | x::xs ->
    List.fold_left (fun acc ns -> List.map2 (+) acc ns) x xs
    |> monkey_business

let () =
  let monkeys, monkeys_items =
    open_in Sys.argv.(1)
    |> IO.input_all
    |> Str.(split @@ regexp "\n\n")
    |> List.map parse 
    |> List.split
    |> (fun (a, b) -> Array.of_list a, Array.of_list b)
  in
  solve monkeys (Array.copy monkeys_items) |> print_int;
  print_newline ();
  solve ~part2: true monkeys monkeys_items |> print_int;
