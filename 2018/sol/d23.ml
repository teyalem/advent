module P = struct
  type t = int * int * int

  let compare (x, y, z) (a, b, c) =
    match Int.compare x a with
    | 0 ->
      (match Int.compare y b with
       | 0 -> Int.compare z c
       | v -> v)
    | v -> v
end

let argmax compare f = function
  | [] -> failwith "empty list"
  | x::xs ->
    List.fold_left (fun p n ->
        if compare (f p) (f n) < 0 then n else p)
      x xs

let parse str =
  Scanf.sscanf str "pos=<%d,%d,%d>, r=%d"
    (fun x y z r -> (x, y, z), r)

let distance (x, y, z) (a, b, c) =
  abs (x - a) + abs (y - b) + abs (z - c)

let num_in_range xs =
  let pos, r = argmax Int.compare snd xs in
  List.filter (fun (p, _) -> distance pos p <= r) xs
  |> List.length

let in_range (cx, cy, cz) ((x, y, z), r) =
  let f (l, h) m = abs (m - l) + abs (m - h) - (h - l) in
  (f cx x + f cy y + f cz z) / 2 <= r

let split (a, b) =
  let m = (a + b) / 2 in
  [a, m; min (m+1) b, b]

let children (x, y, z) =
  split x
  |> List.concat_map (fun x -> split y |> List.map (fun y -> x, y))
  |> List.concat_map (fun (x, y) -> split z |> List.map (fun z -> x, y, z))

let cube (xl, yl, zl) (xh, yh, zh) =
  (xl, xh), (yl, yh), (zl, zh)

let size (x, _, _) =
  let d (l, h) = h - l + 1 in
  d x

let point (x, y, z) =
  fst x, fst y, fst z

(* https://www.reddit.com/r/adventofcode/comments/a8s17l/comment/ecfmpy0/?utm_source=share&utm_medium=web2x&context=3 *)
let find_maxbot xs =
  let ocube s =
    let minp = ~-s, ~-s, ~-s and maxp = s, s, s in
    cube minp maxp
  in
  let s = ref 1 in
  while not @@ List.for_all (in_range @@ ocube !s) xs do
    s := 2 * !s
  done;

  let ( let* ) = Option.bind in
  let module M = Map.Make(P) in 
  let q = ref M.empty in

  let numbot c = List.(length @@ filter (in_range c) xs) in
  (* I don't know why this works. *)
  let score n s c = n, -s, Int.neg @@ distance (point c) (0, 0, 0) in

  let add c =
    let n = numbot c and s = size c in
    q := !q |> M.add (score n s c) (n, s, c)
  in
  let next () =
    let* k, v = M.max_binding_opt !q in
    q := M.remove k !q;
    Some v
  in

  let rec aux () =
    let* _, s, c = next () in
    if s = 1 then
      Option.some @@ distance (0, 0, 0) @@ point c
    else begin
      children c |> List.iter add;
      aux ()
    end
  in
  add @@ ocube !s;
  aux ()

let () =
  let data =
    open_in Sys.argv.(1)
    |> In_channel.input_all
    |> String.trim
    |> String.split_on_char '\n'
    |> List.map parse
  in 
  (* PART 1 *)
  num_in_range data |> print_int;
  print_newline ();

  (* PART 2 *)
  find_maxbot data |> Option.iter print_int
