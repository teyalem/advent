open Ut

(* Disclaimer : I burrowed the algorithm from somewhere. *)

type t =
  | Leaf of int
  | Node of t * t

let leaf n = Leaf n
let cons a b = Node (a, b)

let num = function
  | Leaf n -> n
  | _ -> assert false

let rec inc_ll x = function
  | Leaf n -> Leaf (x + n)
  | Node (a, b) -> cons (inc_ll x a) b

let rec inc_rr x = function
  | Leaf n -> Leaf (x + n)
  | Node (a, b) -> cons a (inc_rr x b)

let rec explode l = function
  | Leaf n -> None
  | Node (a, b) ->
    if l >= 4 then
      Some (Leaf 0, num a, num b)
    else
      match explode (l+1) a with
      | Some (a, ln, rn) -> Some (cons a (inc_ll rn b), ln, 0)
      | None -> begin
          match explode (l+1) b with
          | Some (b, ln, rn) -> Some (cons (inc_rr ln a) b, 0, rn)
          | None -> None
        end

let rec split = function
  | Leaf n ->
    if n >= 10
    then Some (cons (leaf (n/2)) (leaf (n/2 + n mod 2)))
    else None
  | Node (a, b) ->
    match split a with
    | Some a -> Some (cons a b)
    | None -> begin
        match split b with
        | Some b -> Some (cons a b)
        | None -> None
      end

let rec reduce n =
  match explode 0 n with
  | Some (n, _, _) -> reduce n
  | None -> begin
      match split n with
      | Some n -> reduce n
      | None -> n
    end

let add a b = reduce @@ cons a b

let rec magnitude = function
  | Leaf n -> n
  | Node (a, b) -> 3*magnitude a + 2*magnitude b

let parse str =
  let str = String.to_seq str |> List.of_seq in
  let tonum c = Char.(code c - code '0') in
  let rec aux ns = function
    | [] -> List.hd ns
    | '[' :: xs | ',' :: xs -> aux ns xs
    | ']' :: xs -> begin match ns with
        | a :: b :: ns ->
          aux (cons b a :: ns) xs
        | _ -> assert false
      end
    | n :: xs ->
      let l = leaf @@ tonum n in
      aux (l::ns) xs
  in
  aux [] str

let () =
  let data = IO.read_lines () |> List.map parse in
  begin
    (* PART 1 *)
    List.(fold_left add (hd data) (tl data))
    |> magnitude
    |> Printf.printf "%d\n";

    (* PART 2 *)
    List.concat_map (fun n ->
        List.filter_map
          (fun m -> if n = m then None else Some (n, m)) data)
      data
    |> List.map (fun (a, b) -> add a b)
    |> List.map magnitude
    |> List.fold_left max Int.min_int
    |> Printf.printf "%d\n";
  end
