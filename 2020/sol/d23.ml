(* Cup Game *)
module Game = struct
  type t = { mutable cur: int;
             cups: int array; }

  let load l =
    let arr = Array.make (List.length l + 1) 0 in
    let first = List.hd l
    and rest = List.tl l in
    let last = List.fold_left (fun p n -> arr.(p) <- n; n) first rest in
    arr.(last) <- first;
    { cur = first;
      cups = arr; }

  let extend len g =
    let pre_len = Array.length g.cups in
    let arr =
      Array.init
        (len + 1)
        (fun i -> if i < pre_len then g.cups.(i) else i+1)
    in
    arr.(len) <- g.cur;
    arr.(1) <- pre_len; (* dirty hardcoding *)

    { cur = g.cur;
      cups = arr; }

  let insert_three dest (a, _, c) g =
    let d_next = g.cups.(dest) in
    g.cups.(dest) <- a;
    g.cups.(c) <- d_next

  let find_dest (a, b, c) g =
    let find_from n =
      let result = ref 0 in
      match
        for i = n - 1 downto 1 do
          if i = a || i = b || i = c
          then ()
          else (result := i; raise Exit)
        done
      with () -> None
         | exception Exit -> Some !result
    in

    match find_from g.cur with
    | None -> Option.get @@ find_from @@ Array.length g.cups
    | Some result -> result

  let move g = (* assume queue is not empty *)
    (* take three *)
    let a = g.cups.(g.cur) in
    let b = g.cups.(a) in
    let c = g.cups.(b) in
    let next_cur = g.cups.(c) in
    let three = a, b, c in

    (* find destination cup *)
    let dest = find_dest three g in
    insert_three dest three g;

    g.cups.(g.cur) <- next_cur;
    g.cur <- next_cur

  let run n g =
    for _ = 1 to n do
      move g
    done

end

let test = "389125467"
let input = "739862541"

let main () =
  let data = input |> String.to_seq
             |> Seq.map (fun c -> String.make 1 c |> int_of_string)
             |> List.of_seq
  in begin
    (* PART 1 *)
    let g = Game.load data in
    print_endline "game loaded";
    Game.run 100 g;
    print_endline "game done";

    let next = ref 1 in
    for _ = 1 to 8 do
      print_int g.cups.(!next);
      next := g.cups.(!next)
    done;

    print_newline ();

    (* PART 2 *)
    let g = Game.(load data |> extend 1_000_000) in

    Game.run 10_000_000 g;

    let a = g.cups.(1) in
    let b = g.cups.(a) in
    print_int (a*b)

  end

let () = main ()
