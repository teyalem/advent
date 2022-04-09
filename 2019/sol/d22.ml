open Ut

module Deal = struct
  type t = Reverse | Cut of int | Increment of int

  let parse str =
    let (>|) (a: 'a -> 'b option) (b: 'a -> 'b option) : ('a -> 'b option) =
      fun x ->
        match a x with
          Some c -> Some c
        | None -> b x
    in

    let parse_reverse_opt str = 
      if str = "deal into new stack"
      then Some Reverse
      else None
    in
    
    let parse_cut_opt str =
      try Scanf.sscanf str "cut %d" (fun n -> Some (Cut n))
      with _ -> None
    in

    let parse_inc_opt str =
      try Scanf.sscanf str "deal with increment %d" (fun n -> Some (Increment n))
      with _ -> None
    in

    (parse_reverse_opt >| parse_cut_opt >| parse_inc_opt) str
    |> Option.get

end

let cycle n m =
  let a = n mod m in
  if a > 0
  then a
  else (m + a) mod m

module Deck = struct
  type t = int array

  let size = 10007

  let init () = Array.init size (fun n -> n)

  let reverse deck =
    Array.init size (fun i -> deck.(size-1 - i))

  let cut deck n =
    Array.init size (fun i -> deck.(cycle (n + i) size))

  let increment deck n =
    let arr = Array.make size 0 in
    for i = 0 to size - 1 do
      let to_i = cycle (i * n) size in
      arr.(to_i) <- deck.(i)
    done;
    arr

  let perform deck deal =
    let open Deal in
    match deal with
      Reverse -> reverse deck
    | Cut n -> cut deck n
    | Increment n -> increment deck n

  let perform_deals deck deals =
    List.fold_left perform deck deals

  let find_position deck n =
    let result = ref 0 in
    for i = 0 to size - 1 do
      if deck.(i) = n
      then result := i
      else ()
    done;
    !result

end

let mod_inv n modn =
  Math.pow_mod n (modn-2) modn

module Tracker = struct
  type t = { size: int;
             offset: int;
             inc: int; }

  let make size offset inc = { size = size;
                               offset = offset;
                               inc = inc; }

  let init size =
    make size 0 1

  let update t offset inc =
    make t.size offset inc

  let reverse t = 
    let inc = t.inc * -1 in
    update t (t.offset + inc) inc

  let cut t n =
    update t (t.offset + t.inc * n) t.inc

  let increment t n =
    update t t.offset (t.inc * mod_inv n t.size)

  let perform t deal =
    let open Deal in
    match deal with
      Reverse -> reverse t
    | Cut n -> cut t n
    | Increment n -> increment t n

  let perform_deals t deals =
    List.fold_left perform t deals

  let repeat t i =
    let inc = Math.pow_mod t.inc i t.size in
    let offset = t.offset * (1 - inc) * mod_inv ((1 - t.inc) mod t.size) t.size in
    update t (offset mod t.size) inc

  let index t n =
    (n * t.inc + t.offset) mod t.size

end

let () =
  let data = IO.read_lines () |> List.map Deal.parse in
  begin
    (* PART 1 *)
    let deck = Deck.perform_deals (Deck.init ()) data in
    Deck.find_position deck 2019 |> print_int;

    print_newline ();

    (* PART 2 *)
    (* TODO: I can't understand it right now, need to go back to this after a while.... *)
    (* https://github.com/twattanawaroon/adventofcode/blob/master/2019/q22b.py *)
    let t = Tracker.init 119315717514047
            |> (fun t -> Tracker.perform_deals t data)
    in
    let t = Tracker.repeat t 101741582076661 in
    Tracker.index t 2020 |> print_int
  end
