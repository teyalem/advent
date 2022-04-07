open Ut

type ingredient = {
  capacity: int;
  durability: int;
  flavor: int;
  texture: int;
  calories: int;
}

let make_ig capacity durability flavor texture calories =
  { capacity; durability; flavor; texture; calories; }

let map2 f a b =
  { capacity = f a.capacity b.capacity;
    durability = f a.durability b.durability;
    flavor = f a.flavor b.flavor;
    texture = f a.texture b.texture;
    calories = f a.calories b.calories; }

let add = map2 Int.add
let mult a b =
  let bi = make_ig b b b b b in
  map2 Int.mul a bi

let zero = make_ig 0 0 0 0 0

let parse str =
  Scanf.sscanf str "%s@: capacity %d, durability %d, flavor %d, texture %d, calories %d"
    (fun name cap dur fla tex cal -> name, make_ig cap dur fla tex cal)

let calc_score_calories ingreds amounts =
  List.map2 mult ingreds amounts
  |> List.fold_left add zero
  |> (fun { capacity; durability; flavor; texture; calories } ->
      let c = max capacity 0
      and d = max durability 0
      and f = max flavor 0
      and t = max texture 0 in
      c*d*f*t, calories)

let cookie_score_calories ingreds =
  Seq.init 101 (fun a ->
      Seq.init (101-a) (fun b ->
          Seq.init (101-a-b) (fun c ->
              Seq.init (101-a-b-c) (fun d ->
                  [ a; b; c; d ]))))
  |> Seq.concat
  |> Seq.concat
  |> Seq.concat
  |> Seq.map (fun ns -> calc_score_calories ingreds ns)

let () =
  let data = IO.read_lines () |> List.map parse |> List.map snd in
  let rsc = cookie_score_calories data |> Seq.memoize in
  begin
    (* PART 1 *)
    Seq.map fst rsc
    |> Seq.fold_left max min_int
    |> Printf.printf "%d\n";

    (* PART 2 *)
    Seq.filter_map (fun (s, c) -> if c = 500 then Some s else None) rsc
    |> Seq.fold_left max min_int
    |> Printf.printf "%d\n";
  end
