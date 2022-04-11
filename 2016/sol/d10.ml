open Ut

type inst =
  | Value of int * bot
  | Gives of bot * out * out

and bot = int
and out =
  | Bot of bot
  | Output of int

let value_to_bot v b =
  Value (v, b)

let out o n =
  if o = "bot" then Bot n
  else if o = "output" then Output n
  else assert false

let bot_dists b o1 n1 o2 n2 =
  Gives (b, out o1 n1, out o2 n2)

let parse str =
  if String.starts_with ~prefix: "value" str then
    Scanf.sscanf str "value %d goes to bot %d" value_to_bot
  else
    Scanf.sscanf str "bot %d gives low to %s %d and high to %s %d" bot_dists

let simul_bots np insts =
  let module H = Hashtbl in
  let bots : (bot, int list) H.t = H.create 100 in
  let outputs : (int, int) H.t = H.create 100 in
  let get b = H.find_opt bots b |> Option.value ~default: [] in
  let found = ref None in
  let give b v =
    match b with
    | Bot b -> let l = get b in H.replace bots b (v::l)
    | Output i -> H.replace outputs i v
  in
  let perform = function
    | Value (v, b) -> give (Bot b) v
    | Gives (b, lo, hi) ->
      let ns = get b in
      if List.length ns = 2 then
        let l, h =
          let n, m = List.(nth ns 0, nth ns 1) in
          min n m, max n m
        in
        if np = (l, h) then found := Some b;
        give lo l;
        give hi h
  in
  let rec aux insts =
    let insts = insts |> List.filter_map (fun inst ->
        match inst with
        | Value _ -> perform inst; None
        | Gives (b, _, _) ->
          if (List.length @@ get b) = 2
          then begin perform inst; None end
          else Some inst)
    in
    if List.length insts > 0 then aux insts
  in
  aux insts;
  !found, outputs

let () =
  let data = IO.read_lines () |> List.map parse in
  let bot, outputs = simul_bots (17, 61) data in
  begin
    (* PART 1 *)
    bot |> Option.get |> Printf.printf "%d\n";

    (* PART 2 *)
    let a i = Hashtbl.find outputs i in
    a 0 * a 1 * a 2 |> Printf.printf "%d\n";
  end
