open Ut

type value =
  | Reg of string
  | Int of int

let parse str : (string * string * value) =
  Scanf.sscanf str "%s %s %s" (fun c a b ->
      if c = "inp" then
        c, a, Int 0
      else
        let b =
          try Int (int_of_string b)
          with _ -> Reg b
        in
        c, a, b)

type p =
  | Push of int
  | Pop of int

let transform p =
  let open List in
  let split ps : (string * string * value) list list =
    fold_left (fun n (c, _, _ as inst) ->
        if c = "inp" then
          []::n
        else
          match n with
          | [] -> assert false
          | x::xs -> 
            (inst::x) :: xs)
      []
      ps
    |> rev
  in

  let to_stackinst chunk =
    if exists (fun (c, a, b) -> (c, a, b) = ("div", "z", Int 1)) chunk
    then (* push *)
      chunk
      |> find_map (fun (c, a, b) ->
          if (c, a) = ("add", "y") then Some b else None)
      |> Option.get
      |> (fun b -> match b with Int i -> Push i | _ -> assert false)
    else (* pop *)
      rev chunk
      |> filter_map (fun (c, a, b) ->
          if (c, a) = ("add", "x") then Some b else None)
      |> find_map (function
          | Int i -> Some (Pop i)
          | _ -> None)
      |> Option.get
  in

  let rec gen_constraints i stack consts = function
    | [] -> consts
    | Push n :: xs ->
      gen_constraints (i+1) ((i, n) :: stack) consts xs
    | Pop n :: xs ->
      let p, m = hd stack in
      let const = p, i, m+n in
      gen_constraints (i+1) (tl stack) (const::consts) xs
  in
  split p
  |> map to_stackinst
  |> gen_constraints 0 [] []

(* make max/min number from given constraints *)
let max_monad ns =
  let a = Array.make 14 0 in
  List.iter (fun (i, j, n) ->
      a.(i) <- 9; a.(j) <- 9;
      if n > 0 then a.(i) <- 9-n else a.(j) <- 9+n)
    ns;
  Array.fold_left (fun p n -> 10*p + n) 0 a

let min_monad ns =
  let a = Array.make 14 0 in
  List.iter (fun (i, j, n) ->
      a.(i) <- 1; a.(j) <- 1;
      if n > 0 then a.(j) <- 1+n else a.(i) <- 1-n)
    ns;
  Array.fold_left (fun p n -> 10*p + n) 0 a

let () =
  let data = IO.read_lines () |> List.map parse in
  let const = transform data in
  begin
    (* PART 1 *) max_monad const |> Printf.printf "%d\n";
    (* PART 2 *) min_monad const |> Printf.printf "%d\n";
  end
