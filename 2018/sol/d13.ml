module IP = struct
  type t = int * int

  let compare (a, b) (x, y) =
    match Int.compare b y with
    | 0 -> Int.compare a x
    | v -> v

  let equal a b = compare a b = 0

  let clockwise (x, y) = -y, x
  let counterwise (x, y) = y, -x
  let slash (x, y) = -y, -x
  let backslash (x, y) = y, x
  let flip (x, y) = -x, -y

  let add (a, b) (x, y) = a+x, b+y

  let print (x, y) = Printf.printf "%d,%d" x y

  let is_dir (x, y) = abs x + abs y = 1
end

module M = Map.Make(IP)

module Track = struct
  type t = string array

  let get t (x, y) =
    t.(y).[x]
end

module Cart = struct
  type t = IP.t * int

  let of_char c =
    (match c with
     | '^' ->  0, -1 | 'v' ->  0,  1
     | '<' -> -1,  0 | '>' ->  1,  0
     | _ -> assert false),
    0

  let of_char_opt c =
    try Some (of_char c) with _ -> None

  let to_tile = function
    | '^' | 'v' -> '|'
    | '<' | '>' -> '-'
    | _ -> assert false

  let update track pos (v, i) =
    assert (IP.is_dir v);
    let pos = IP.add pos v in
    pos,
    match Track.get track pos with
    | '/' -> IP.slash v, i
    | '\\' -> IP.backslash v, i
    | '+' ->
      (match i with
       | 0 -> (* left *) IP.counterwise v
       | 1 -> (* straight *) v
       | 2 -> (* right *) IP.clockwise v
       | _ -> assert false),
      (i + 1) mod 3
    | _ ->  v, i
end

let parse_line y str : string * (IP.t * Cart.t) list =
  let cs = ref [] in
  let line =
    Bytes.of_string str
    |> Bytes.mapi (fun x c ->
        match Cart.of_char_opt c with
        | None -> c
        | Some cart ->
          cs := ((x, y), cart) :: !cs;
          Cart.to_tile c)
    |> Bytes.to_string
  in
  line, !cs

let parse str : Track.t * Cart.t M.t =
  let maps, carts =
    String.split_on_char '\n' str
    |> List.mapi parse_line
    |> List.split
  in
  Array.of_list maps,
  List.concat carts |> List.to_seq |> M.of_seq

let update_carts1 track m =
  let rec aux acc = function
    | [] -> Either.Left acc
    | (p, c)::xs ->
      let p, c = Cart.update track p c in
      if M.mem p acc || List.exists (IP.equal p) @@ List.map fst xs
      then Right p
      else aux (M.add p c acc) xs
  in
  aux M.empty @@ M.bindings m

let find_crash1 track carts =
  let rec aux m =
    match update_carts1 track m with
    | Either.Left m -> aux m
    | Right pos -> pos
  in
  aux carts

let update_carts2 track m =
  let rec aux acc = function
    | [] -> acc
    | (p, c)::xs ->
      let p, c = Cart.update track p c in
      let acc, xs =
        if M.mem p acc || List.exists (IP.equal p) @@ List.map fst xs
        then M.remove p acc, List.filter (fun (n, _) -> n <> p) xs
        else M.add p c acc, xs
      in
      aux acc xs
  in
  aux M.empty @@ M.bindings m

let find_crash2 track carts =
  let rec aux m =
    let m = update_carts2 track m in
    match M.bindings m with
    | [] -> assert false
    | [p, _] -> p
    | _ -> aux m
  in
  aux carts

let () =
  let track, carts = In_channel.input_all stdin |> parse in
  (* PART 1 *)
  find_crash1 track carts |> IP.print;
  print_newline ();

  (* PART 2 *)
  find_crash2 track carts |> IP.print;
