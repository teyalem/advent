open Ut

type ani = {
  hp : int;
  mana : int;
  armor : int;
  dmg : int;
}

let make_ani hp mana armor dmg =
  { hp; mana; armor; dmg }

let parse_boss str =
  Scanf.sscanf str "Hit Points: %d\nDamage: %d"
    (fun hp dmg -> make_ani hp 0 0 dmg)

let me = make_ani 50 500 0 0

type effect = string * int * (int -> ani -> ani -> ani * ani)

let shield_eff : effect =
  let aux c me boss =
    if c = 0 then
      { me with armor = me.armor - 7 }, boss
    else me, boss
  in
  "Shield", 6, aux

let poison_eff : effect =
  let aux _ me boss =
    me, { boss with hp = boss.hp - 3 }
  in
  "Poison", 6, aux

let recharge_eff : effect =
  let aux _ me boss =
    { me with mana = me.mana + 101 }, boss
  in
  "Recharge", 5, aux

type state = ani * ani * effect list 

type magic = {
  name : string;
  cost : int;
  f : state -> state;
}

let magic name cost f =
  { name; cost; f }

let magics = [
  magic "Magic Missile" 53 (fun (m, b, e) -> m, {b with hp = b.hp-4}, e);
  magic "Drain"         73 (fun (m, b, e) -> {m with hp = m.hp+2},
                                             {b with hp = b.hp-2}, e);
  magic "Shield"        113 (fun (m, b, e) ->
      { m with armor = m.armor + 7 }, b, shield_eff::e);
  magic "Poison"        173 (fun (m, b, e) -> m, b, poison_eff::e);
  magic "Recharge"      229 (fun (m, b, e) -> m, b, recharge_eff::e);
]

let apply_effects (me, boss, effs) : state =
  let me, boss, effs =
    List.fold_left (fun (me, boss, effs) (name, cnt, eff) ->
        let cnt = cnt - 1 in
        let me, boss = eff cnt me boss in
        me, boss, if cnt = 0 then effs else (name, cnt, eff) :: effs)
      (me, boss, [])
      effs
  in
  me, boss, List.rev effs

let cast_magic (me, boss, effs) { cost; f; _ } =
  let me = { me with mana = me.mana - cost } in
  f (me, boss, effs)

let boss_attack (me, boss, effs) =
  let dmg = max (boss.dmg - me.armor) 1 in
  { me with hp = me.hp - dmg }, boss, effs

let is_boss_defeated (_, boss, _) =
  boss.hp <= 0

let is_me_dead (me, _, _) =
  me.hp <= 0

let battle_part1 state magic =
  (* me attack *)
  let state = cast_magic state magic in
  if is_boss_defeated state then Either.Right "Me"
  else
    (* upkeep *)
    let state = apply_effects state in
    if is_boss_defeated state then Either.Right "Me"
    else
      (* boss attack *)
      let state = boss_attack state in
      (* upkeep *)
      let state = apply_effects state in
      if is_me_dead state
      then Right "Boss"
      else Left state

let selfharm (me, boss, effs) =
  { me with hp = me.hp - 1 }, boss, effs

let battle_part2 state magic =
  (* me attack *)
  let state = cast_magic state magic in
  if is_boss_defeated state then Either.Right "Me"
  else
    (* upkeep *)
    let state = apply_effects state in
    if is_boss_defeated state then Either.Right "Me"
    else
      (* boss attack *)
      let state = boss_attack state in
      (* player upkeep *)
      let state = selfharm state in
      let state = apply_effects state in
      if is_me_dead state
      then Right "Boss"
      else Left state

let usable_magics (me, _, effs) =
  let open List in
  magics
  |> filter (fun m -> m.cost <= me.mana)
  |> filter (fun m -> not @@ exists (fun (name, _, _) -> name = m.name) effs)

let make_battle battle =
  (module struct
    type space = unit
    type nonrec state = (state, string) Either.t
    type data = unit
    type weight = int

    let data_id = ()

    let is_end () (s, _) =
      match s with
      | Either.Right "Me" -> true
      | _ -> false

    let neighbors () (s, _) =
      match s with
      | Either.Left s ->
        usable_magics s
        |> List.map (fun m -> m.cost, battle s m, ())
      | Right _ -> []
  end
  : Pathfind.WeightedGraph
    with type space = unit
     and type state = (state, string) Either.t
     and type data = unit
     and type weight = int)

module OrderedState = struct
  type t = (state, string) Either.t

  let compare a b =
    Hashtbl.(Int.compare (hash a) (hash b))
end

let find_least_mana_part1 state =
  Pathfind.dijkstra (module OrderedState) (module Int)
    (make_battle battle_part1)
    ()
    ~start: (Either.Left state)
  |> fst

let find_least_mana_part2 state =
  let state = selfharm state in
  Pathfind.dijkstra (module OrderedState) (module Int)
    (make_battle battle_part2)
    ()
    ~start: (Either.Left state)
  |> fst

let () =
  let boss = IO.read_all () |> parse_boss in
  let state = me, boss, [] in
  begin
    (* PART 1 *)
    find_least_mana_part1 state |> Printf.printf "%d\n";

    (* PART 2 *)
    find_least_mana_part2 state |> Printf.printf "%d\n";
  end
