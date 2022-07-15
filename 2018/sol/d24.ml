module G = struct
  type t = {
    side : army_type;
    hp : int;
    mutable dmg : int;
    dmg_type : string;
    weak : string list;
    immune : string list;
    initiative : int;
    mutable number : int;
  }
  and army_type = Immune | Infection

  let side_to_string = function
    | Immune -> "Immune system"
    | Infection -> "Infection"

  let side x = x.side
  let initiative x = x.initiative
  let unit_number x = x.number

  let make side number hp wi dmg dmg_type initiative =
    let f t = List.assoc_opt t wi |> Option.value ~default: [] in
    { side; hp; dmg; dmg_type;
      weak = f "weak";
      immune = f "immune";
      initiative;
      number; }

  let copy ({ dmg; number; _ } as g) =
    { g with dmg; number }

  let effective_power { number; dmg; _ } =
    number * dmg

  let is_dead { number; _ } =
    number <= 0

  let calc_damage attacker defender =
    let t = attacker.dmg_type in
    let multiplier =
      if List.exists ((=) t) defender.immune then 0
      else if List.exists ((=) t) defender.weak then 2
      else 1
    in
    multiplier * effective_power attacker

  let take_damage g dmg =
    let d = dmg / g.hp in
    g.number <- g.number - d;
    d

  let boost n g =
    g.dmg <- g.dmg + n

  let compare a b =
    match Int.compare (effective_power a) (effective_power b) with
    | 0 -> Int.compare a.initiative b.initiative
    | v -> v

  let print g =
    Printf.printf "%s %d units each with %d hit points (immune to %s; weak to %s) with an attack
    that does %d %s damage at initiative %d\n"
      (side_to_string g.side) g.number g.hp
      (String.concat " " g.immune) (String.concat " " g.weak)
      g.dmg g.dmg_type g.initiative

  let parse =
    let aux str =
      String.split_on_char ';' str
      |> List.map String.trim
      |> List.map (fun s ->
          Scanf.sscanf s "%s to %s@!" (fun t l ->
              t, String.(split_on_char ',' l |> List.map trim)))
    in
    fun side str ->
      Scanf.sscanf str
        "%d units each with %d hit points %s@!"
        (fun n hp rest ->
           if rest.[0] = '(' then
             Scanf.sscanf rest "(%s@) with an attack that does %d %s damage at initiative %d"
               (fun wi d dt i -> make side n hp (aux wi) d dt i)
           else
             Scanf.sscanf rest "with an attack that does %d %s damage at initiative %d"
               (fun d dt i -> make side n hp [] d dt i))
end

type groups = G.t array

let group_list gs =
  Array.to_list gs
  |> List.mapi (fun i x -> i, x)

let enemy_sorted gs attacker =
  group_list gs
  |> List.filter (fun (_, x) -> G.side x <> G.side attacker)
  |> List.map (fun (i, x) -> i, G.calc_damage attacker x, x)
  |> List.sort (fun (_, da, a) (_, db, b) ->
      match Int.compare db da with
      | 0 -> G.compare b a
      | v -> v)

let select_target gs targets (i, g) =
  match
    enemy_sorted gs g
    |> List.filter (fun (n, _, _) ->
        List.for_all (fun (_, m) -> n <> m) targets)
  with
  | [] -> None
  | (j, d, _) :: _ -> if d = 0 then None else Some (i, j)

let selection_phase gs =
  group_list gs
  |> List.sort (fun (_, a) (_, b) -> G.compare b a)
  |> List.fold_left (fun targets x ->
      match select_target gs targets x with
      | None -> targets
      | Some n -> n :: targets)
    []
  |> List.rev

let attack_phase gs atk_list =
  atk_list
  |> List.sort (fun (a, _) (b, _) ->
      Int.compare (G.initiative gs.(b)) (G.initiative gs.(a)))
  |> List.map (fun (attacker, defender) ->
      if not @@ G.is_dead gs.(attacker) then
        let dmg = G.calc_damage gs.(attacker) gs.(defender) in
        G.take_damage gs.(defender) dmg
      else 0)

let remove_deads gs =
  Array.to_seq gs
  |> Seq.filter (Fun.negate G.is_dead)
  |> Array.of_seq

let print_groups =
  Array.iter G.print

let fight gs =
  let sel = selection_phase gs in
  if sel = [] then None
  else
    let atk = attack_phase gs sel in
    if List.for_all ((=) 0) atk then None
    else Some (remove_deads gs)

let simul gs =
  let rec aux gs =
    let u = G.side gs.(0) in
    if Array.for_all (fun g -> G.side g = u) gs
    then Some gs
    else Option.bind (fight gs) aux
  in
  aux gs

let total_units gs =
  Array.fold_left (fun acc g -> acc + G.unit_number g) 0 gs

let copy_groups =
  Array.map G.copy

let find_minimum_boost gs =
  let score gs =
    (match G.side gs.(0) with G.Immune -> 1 | Infection -> -1)
    * total_units gs
  in
  let test n =
    let t = copy_groups gs in
    t |> Array.iter (fun g -> if G.side g = G.Immune then G.boost n g);
    simul t |> Option.map score
  in

  let rec aux i = (* little slow, but it works *)
    match test i with
    | Some n when n > 0 -> n
    | _ -> aux (i+1)
  in
  aux 0

let read lines =
  let rec aux side = function
    | [] -> []
    | "" :: xs -> aux side xs
    | "Immune System:" :: xs -> aux G.Immune xs
    | "Infection:" :: xs -> aux G.Infection xs
    | x :: xs -> G.parse side x :: aux side xs
  in
  aux G.Immune lines
  |> Array.of_list

let () =
  let data =
    open_in Sys.argv.(1)
    |> In_channel.input_all
    |> String.split_on_char '\n'
    |> read
  in
  (* PART 1 *)
  data |> copy_groups |> simul |> Option.get |> total_units |> print_int;
  print_newline ();

  (* PART 2 *)
  find_minimum_boost data |> print_int;
