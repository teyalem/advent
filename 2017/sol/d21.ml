open Ut

module T = struct
  type t = On | Off

  let of_char = function
    | '#' -> On
    | '.' -> Off
    | _ -> assert false

  let to_char = function
    | On -> '#'
    | Off -> '.'

  let default = Off
end

module B = Block.Make(T)

let pattern = [
  ".#.";
  "..#";
  "###";
] |> B.parse

let split b size =
  let d = B.dimx b in
  let n = d / size in
  List.init n (fun y ->
      List.init n (fun x ->
          B.sub b (x*size, y*size) (size, size)))

let map2d f =
  List.map @@ List.map f

let rotate_clockwise b =
  Mat.init (B.dimx b) (B.dimy b) (fun x y -> B.(get b y (dimx b-1 - x)))

let rotate_counterclockwise b =
  Mat.init (B.dimx b) (B.dimy b) (fun x y -> B.(get b (dimy b-1 - y) x))

let flip_x b =
  Mat.init (B.dimx b) (B.dimy b) (fun x y -> B.(get b (dimx b-1 - x) y))

let flip_y b =
  Mat.init (B.dimx b) (B.dimy b) (fun x y -> B.(get b x (dimy b-1 - y)))

let enhance es b =
  List.find_opt (fun (d, _) -> List.exists ((=) b) d) es
  |> Option.map snd
  |> (function
      | None -> B.print b; assert false
      | Some n -> n)

let process es b =
  let d = B.dimx b in
  split b (if d mod 2 = 0 then 2 else if d mod 3 = 0 then 3 else assert false)
  |> map2d (enhance es)
  |> Mat.concat

let make_all_combi b =
  [ Fun.id;
    rotate_clockwise;
    rotate_counterclockwise;
    flip_x;
    flip_y;
    (fun b -> rotate_clockwise @@ rotate_clockwise b);
    (fun b -> flip_x @@ rotate_clockwise b);
    (fun b -> flip_x @@ rotate_counterclockwise b);
    (fun b -> flip_y @@ rotate_clockwise b);
    (fun b -> flip_y @@ rotate_counterclockwise b);
  ] |> List.map (fun f -> f b)

let make_enchance_rule =
  List.map (fun (b, a) -> make_all_combi b, a)

let parse_rule str =
  let parse_b str = String.split_on_char '/' str |> B.parse in
  Scanf.sscanf str "%s => %s" (fun b a -> parse_b b, parse_b a)

let () =
  let data = IO.read_lines () |> List.map parse_rule |> make_enchance_rule in
  let f n =
    Seq.iterate (process data) pattern
    |> Seq.drop n
    |> Seq.take 1
    |> Seq.map (B.count_occur T.On)
    |> Seq.iter print_int;
    print_newline ()
  in
  (* PART 1 *) f 5;
  (* PART 2 *) f 18;
