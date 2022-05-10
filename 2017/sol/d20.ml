open Ut

module Vec3 = struct
  type t = int * int * int

  let make x y z = x, y, z

  let of_string str =
    Scanf.sscanf str " %d, %d, %d" make

  let add (a, b, c) (x, y, z) =
    a+x, b+y, c+z

  let compare (a, b, c) (x, y, z) =
    match Int.compare a x with
    | 0 ->
      (match Int.compare b y with
       | 0 -> Int.compare c z
       | v -> v)
    | v -> v

  let abs (x, y, z) =
    make (abs x) (abs y) (abs z)

  let abs_compare a b =
    compare (abs a) (abs b)

  let print (x, y, z) =
    Printf.printf "<%d, %d, %d>" x y z
end

module Particle = struct
  type t = { p : Vec3.t; v : Vec3.t; a : Vec3.t; }

  let make p v a = { p; v; a }

  let position x = x.p
  let accel x = x.a

  let of_string str =
    let f = Vec3.of_string in
    Scanf.sscanf str "p=<%s@>, v=<%s@>, a=<%s@>" (fun p v a ->
        make (f p) (f v) (f a))

  let compare a b =
    match Vec3.compare a.p b.p with
    | 0 ->
      (match Vec3.compare a.v b.v with
       | 0 -> Vec3.compare a.a b.a
       | v -> v)
    | v -> v

  let abs_compare a b =
    match Vec3.compare a.a b.a with
    | 0 ->
      (match Vec3.compare a.v b.v with
       | 0 -> Vec3.compare a.p b.p
       | v -> v)
    | v -> v

  let step { p; v; a } =
    let v = Vec3.add v a in
    let p = Vec3.add p v in
    make p v a

  let print { p; v; a } =
    let printf = Printf.printf in
    Vec3.(print p; printf " "; print v; printf " "; print a)
end

let remove_collision ps =
  List.sort Particle.compare ps
  |> List.to_seq
  |> Seq.group (fun a b -> Particle.(position a = position b))
  |> Seq.filter (fun s -> Seq.length s = 1)
  |> Seq.map (fun s -> Seq.uncons s |> Option.get |> fst)
  |> List.of_seq

let track_collision ps =
  let rec aux i ps =
    if i = 1000 then ps (* I don't know end condition; just iterate a lot *)
    else
      aux (i+1) @@ remove_collision @@ List.map Particle.step ps
  in
  aux 0 ps |> List.length

let () =
  let data = IO.read_lines () |> List.map Particle.of_string in
  (* PART 1 *)
  data
  |> List.mapi (fun i x -> i, x)
  |> List.filter (fun (_, x) -> Particle.accel x = (0, 0, 0))
  |> List.sort (fun (_, a) (_, b) -> Particle.abs_compare a b)
  |> List.hd
  |> fst
  |> print_int;

  print_newline ();

  (* PART 2 *)
  track_collision data |> print_int
