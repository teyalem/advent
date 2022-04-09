open Ut

let gravity a b =
  match compare a b with
  | 0 -> 0, 0 (* a = b *)
  | -1 -> +1, -1 (* a < b *)
  | 1 -> -1, +1 (* a > b *)
  | _ -> assert false

module Vec3D = struct
  type t = { x: int;
             y: int;
             z: int; }

  let make x y z = { x = x; y = y; z = z }

  let x c = c.x
  let y c = c.y
  let z c = c.z

  let map f a = make (f a.x) (f a.y) (f a.z)

  let map2 f a b = { x = f a.x b.x;
                     y = f a.y b.y;
                     z = f a.z b.z; }

  let fold f a = f (f a.x a.y) a.z

  let add a b = map2 (+) a b

  let compare a b =
    match compare a.x b.x with
    | 0 -> begin
        match compare a.y b.y with
        | 0 -> compare a.z b.z
        | c -> c
      end
    | c -> c

end

module Moon = struct
  type t = { mutable position: Vec3D.t;
             mutable velocity: Vec3D.t; }

  let parse str =
    Scanf.sscanf str "<x=%d, y=%d, z=%d>"
      (fun x y z -> { position = Vec3D.make x y z;
                      velocity = Vec3D.make 0 0 0; })

  let position m = m.position
  let velocity m = m.velocity

  let copy { position; velocity } =
    { position; velocity }

  let apply_velocity m =
    m.position <- Vec3D.add m.position m.velocity

  let apply_gravity a b =
    let ga = Vec3D.map2 (fun a b -> gravity a b |> fst) a.position b.position
    and gb = Vec3D.map2 (fun a b -> gravity a b |> snd) a.position b.position in
    a.velocity <- Vec3D.add a.velocity ga;
    b.velocity <- Vec3D.add b.velocity gb

  let potential_energy m =
    Vec3D.map abs m.position |> Vec3D.fold (+)

  let kinetic_energy m =
    Vec3D.map abs m.velocity |> Vec3D.fold (+)

  let total_energy m =
    potential_energy m * kinetic_energy m
end

let rec unique_pairs = function
  | [] | [_] -> assert false
  | [a; b] -> [a, b]
  | x::xs -> List.map (fun b -> x, b) xs @ unique_pairs xs

let rec gcd a b =
  if b = 0
  then a
  else gcd b (a mod b)

let lcm a b =
  let g = gcd a b in
  abs (a*b) / g

let fold f l = List.(fold_left f (hd l) (tl l))

type pv = { mutable p: int; mutable v: int; }

let copy m = { p = m.p; v = m.v; }

let find_loop first_pl =
  let rec loop i prev pvl =
    unique_pairs pvl
    |> List.iter (fun (m1, m2) ->
        let g1, g2 = gravity m1.p m2.p in
        m1.v <- m1.v + g1;
        m2.v <- m2.v + g2);

    List.iter (fun m -> m.p <- m.p + m.v) pvl;
    let pl = List.map (fun m -> m.p) pvl in
    if pl = first_pl && pl = prev
    then i
    else loop (i+1) pl pvl
  in

  let pvl = List.map (fun p -> { p = p; v = 0 }) first_pl in
  loop 1 first_pl pvl

let () =
  let data = IO.read_lines () |> List.map Moon.parse in
  begin
    (* PART 1 *)
    let moons = List.map Moon.copy data in
    for _ = 1 to 1000 do
      unique_pairs moons
      |> List.iter (fun (a, b) -> Moon.apply_gravity a b);
      List.iter Moon.apply_velocity moons
    done;
    List.fold_left (fun p m -> p + Moon.total_energy m) 0 moons
    |> print_int;
    
    print_newline ();

    (* PART 2 *)
    let moons = data in
    let xpl, ypl, zpl =
      List.map (fun m -> Moon.position m) moons
      |> List.map (fun p -> let open Vec3D in x p, y p, z p)
      |> (fun l ->
          List.fold_right
            (fun (x, y, z) (xl, yl, zl) -> x::xl, y::yl, z::zl)
            l ([], [], []))
    in

    fold lcm 
      [ find_loop xpl;
        find_loop ypl;
        find_loop zpl; ]
    |> print_int

  end
