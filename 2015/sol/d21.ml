open Ut
open Seq

type character = {
  hp : int;
  dmg : int;
  armor : int;
}

let me = {
  hp = 100;
  dmg = 0;
  armor = 0;
}

let character_of_string str =
  Scanf.sscanf str "Hit Points: %d\nDamage: %d\nArmor: %d"
    (fun hp dmg armor -> { hp; dmg; armor })

type item = {
  name : string;
  cost : int;
  dmg : int;
  armor : int;
}

let item name cost dmg armor =
  { name; cost; dmg; armor }

let cost { cost; _ } =
  cost

let apply_item (c : character) { dmg; armor; _ } : character =
  { c with dmg = c.dmg + dmg; armor = c.armor + armor }

let weapons = [
  item "Dagger"        8     4       0;
  item "Shortsword"   10     5       0;
  item "Warhammer"    25     6       0;
  item "Longsword"    40     7       0;
  item "Greataxe"     74     8       0;
]
  |> List.to_seq

let armors = [
  item "Naked"         0     0       0;
  item "Leather"      13     0       1;
  item "Chainmail"    31     0       2;
  item "Splintmail"   53     0       3;
  item "Bandedmail"   75     0       4;
  item "Platemail"   102     0       5;
]
  |> List.to_seq

let rings = [
  item "Damage +1"    25     1       0;
  item "Damage +2"    50     2       0;
  item "Damage +3"   100     3       0;
  item "Defense +1"   20     0       1;
  item "Defense +2"   40     0       2;
  item "Defense +3"   80     0       3;
]

let rec combi = function
  | [] -> [[]]
  | x::xs ->
    let cs = combi xs in
    cs @ List.map (fun c -> x::c) cs

let attack (cf : character) (ct : character) =
  { ct with hp = ct.hp - (max (cf.dmg - ct.armor) 1) }

let is_dead { hp; _ } =
  hp <= 0

(* battle simulator *)
let rec battle (n1, c1) (n2, c2) =
  let c2 = attack c1 c2 in
  if is_dead c2 then n1
  else battle (n2, c2) (n1, c1)

let me_costs =
  let ringcoms =
    combi rings
    |> List.filter (fun rs -> List.length rs <= 2)
    |> List.to_seq
  in
  weapons
  |> concat_map (fun w -> map (fun a -> w, a) armors)
  |> concat_map (fun (w, a) -> map (fun r -> w::a::r) ringcoms)
  |> map (fun shoplist ->
      List.fold_left apply_item me shoplist,
      List.map cost shoplist |> List.fold_left (+) 0)

let wins boss (c, _) =
  battle ("Me", c) ("Boss", boss) = "Me"

let find_least_cost_win boss =
  me_costs
  |> filter (wins boss)
  |> map snd
  |> fold_left min max_int

let find_most_cost_lose boss =
  me_costs
  |> filter (Fun.negate @@ wins boss)
  |> map snd
  |> fold_left max min_int

let () =
  let boss = IO.read_all () |> character_of_string in
  begin
    (* PART 1 *)
    find_least_cost_win boss |> Printf.printf "%d\n";

    (* PART 2 *)
    find_most_cost_lose boss |> Printf.printf "%d\n"
  end
