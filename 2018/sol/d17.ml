let dimx arr = Array.length arr.(0)
let dimy arr = Array.length arr

let get arr x y =
  try arr.(y).(x)
  with _ -> '%'

let set arr x y c =
  if 0 <= x && x <= dimx arr && 0 <= y && y <= dimy arr
  then arr.(y).(x) <- c 
  else begin (* debug *)
    Printf.eprintf "cannot set pos %d, %d\n" x y;
    failwith "out of bounds"
  end

(* source: https://www.reddit.com/r/adventofcode/comments/a6wpup/comment/ebys4h0/?utm_source=share&utm_medium=web2x&context=3 *)
let water map (x, y) =
  let dimx, dimy = dimx map, dimy map in
  let stack = Stack.create () in
  let add x y =
    [ 0, 0; 0, -1; 1, 0; 0, 1; -1, 0; ]
    |> List.iter (fun (dx, dy) ->
        let x, y as pos = x+dx, y+dy in
        if 0 <= x && x < dimx && 0 <= y && y < dimy then
          Stack.push pos stack)
  in

  let change x y c = set map x y c; add x y in

  let is_supported x y =
    let rec aux x dx =
      if get map x y = '#' then true
      else
        match get map x (y+1) with
        | '#' | '~' -> aux (x+dx) dx
        | _ -> false
    in
    aux x ~-1 && aux (x+1) 1
  in

  let update (x, y) =
    match get map x y with
    | '|' ->
      if is_supported x y then
        change x y '~'
      else if
        let down = get map x (y+1) in
        down = '~' || down = '#'
      then
        [ -1; 1 ] |> List.iter (fun dx ->
            if get map (x+dx) y = '.' then change (x+dx) y '|')
    | '.' ->
      if get map x (y-1) = '|' then
        change x y '|'
      else if get map (x-1) y = '~' (* left *)
           || get map (x+1) y = '~' (* right *)
           || get map x (y-1) = '~' (* above *)
      then
        change x y '~'
    | '~' | '#' -> ()
    | _ -> assert false
  in

  let rec aux () =
    if Stack.is_empty stack then ()
    else begin
      Stack.pop stack |> update;
      aux ()
    end
  in
  set map x y '|';
  add x y;
  aux ()

let minmax =
  List.fold_left (fun (mi, ma) n -> min mi n, max ma n) (max_int, min_int)

let flatten_pair =
  List.concat_map (fun (a, b) -> [a; b])

let paint ps =
  let minx, maxx = minmax @@ flatten_pair @@ List.map fst ps in
  let miny, maxy = minmax @@ flatten_pair @@ List.map snd ps in
  let dx, dy = maxx - minx + 3, maxy - miny + 1 in
  let mat = Array.make_matrix dy dx '.' in
  ps |> List.iter (fun ((x1, x2), (y1, y2)) ->
      for x = x1 to x2 do
        for y = y1 to y2 do
          set mat (x - minx + 1) (y - miny) '#'
        done
      done);
  (500 - minx + 1, 0), mat

let parse str =
  Scanf.sscanf str "%c=%d, %c=%d..%d" (fun a k _ n m ->
      let v, w = (k, k), (n, m) in
      if a = 'x' then v, w else w, v)

let count mat ch =
  let cnt = ref 0 in
  Array.iter (Array.iter (fun c -> if c = ch then incr cnt)) mat;
  !cnt

let () =
  let data =
    open_in Sys.argv.(1)
    |> In_channel.input_all
    |> String.trim
    |> String.split_on_char '\n'
    |> List.map parse
  in
  let spring, map = paint data in
  water map spring;
  let stale = count map '~'
  and flowing = count map '|' in
  (* PART 1 *)
  print_int @@ stale + flowing;
  print_newline ();

  (* PART 2 *)
  print_int stale;
