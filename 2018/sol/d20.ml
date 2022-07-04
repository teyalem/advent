(* tagless-final *)
module type DoorType = sig
  type t

  val to_int : t -> int
  val str : string -> t
  val candis : t list -> t
  val seq : t list -> t
end

module Door1 = struct
  type t = int

  let to_int = Fun.id
  let str = String.length
  let candis ns =
    if List.exists ((=) 0) ns then 0 (* detour *)
    else List.fold_left max min_int ns
  let seq ns = List.fold_left Int.add 0 ns
end

let move (x, y) = function
  | 'N' -> x, y-1
  | 'S' -> x, y+1
  | 'E' -> x-1, y
  | 'W' -> x+1, y
  | _ -> assert false

module Door2 = struct
  type t =
    | Str of char list
    | Candis of t list
    | Sequence of t list

  let to_int tree =
    let rs = Hashtbl.create 500 in
    let update pos d =
      match Hashtbl.find_opt rs pos with
      | None -> Hashtbl.add rs pos d
      | Some n -> Hashtbl.replace rs pos (min n d)
    in

    let rec aux (pos, d) = function
      | Str cs ->
        List.fold_left (fun (pos, d) c ->
            let pos = move pos c in
            update pos (d+1);
            pos, d+1)
          (pos, d)
          cs
      | Candis xs ->
        let _ps = List.map (aux (pos, d)) xs in
        pos, d
      | Sequence xs ->
        List.fold_left aux (pos, d) xs
    in
    ignore @@ aux ((0, 0), 0) tree;
    Hashtbl.to_seq rs
    |> Seq.map snd
    |> Seq.filter (fun n -> n >= 1000)
    |> Seq.length

  let str s = Str (String.to_seq s |> List.of_seq)
  let candis xs = Candis xs
  let seq xs = Sequence xs
end

let is_alpha c =
  'A' <= c && c <= 'Z'

(* combinator parser utils *)
type 'a pars = char Seq.t -> ('a * char Seq.t) option

let (let*) (a: 'a pars) (b: 'a -> 'b pars) : 'b pars =
  fun seq -> Option.bind (a seq) (fun (x, seq) -> b x seq)

let (let+) (a: 'a pars) (f: 'a -> 'b) : 'b pars =
  fun seq -> Option.map (fun (x, seq) -> f x, seq) (a seq)

let return x seq = Some (x, seq)
let fail _ = None

let oneof ps seq =
  List.find_map (fun f -> f seq) ps

let many f =
  let rec aux seq =
    match f seq with
    | None -> [], seq
    | Some (x, seq) ->
      let xs, seq = aux seq in x :: xs, seq
  in
  fun seq -> Some (aux seq)

let any : char pars = Seq.uncons

let peek seq =
  match Seq.uncons seq with
  | None -> None
  | Some (x, _) -> Some (x, seq)

let letter c : char pars =
  let* x = any in
  fun seq -> if x = c then Some (x, seq) else None

let alphas seq =
  let x = Seq.take_while is_alpha seq |> String.of_seq in
  let seq = Seq.drop_while is_alpha seq in
  if x = "" then None else Some (x, seq)

let parse (type t) (module D: DoorType with type t = t) regex_str =
  let rec regex seq = (let+ xs = many part in D.seq xs) seq
  and part seq = oneof [sequence; options] seq
  and sequence seq = (let+ s = alphas in D.str s) seq
  and options seq =
    let f =
      let* c = peek in
      if c = ')' then fail
      else if c = '|' then let* _ = letter '|' in regex
      else regex
    in
    (let* _ = letter '(' in
     let* xs = many f in
     let* _ = letter ')' in
     return @@ D.candis xs)
      seq
  in

  let aux =
    let* _ = letter '^' in
    let* r = regex in
    let* _ = letter '$' in
    return r
  in
  fst @@ Option.get @@ aux @@ String.to_seq regex_str

let () =
  let data = In_channel.input_all stdin |> String.trim in
  (* PART 1 *)
  parse (module Door1) data |> Door1.to_int |> print_int;
  print_newline ();
  (* PART 2 *)
  parse (module Door2) data |> Door2.to_int |> print_int;
