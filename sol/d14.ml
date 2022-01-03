open Ut

(* f^n(x) *)
let rec repeat f n x =
  if n = 0 then x
  else repeat f (n-1) (f x)

(* last element of xs *)
let last xs =
  List.(nth xs @@ length xs - 1)

let parse_rule str =
  Scanf.sscanf str "%c%c -> %c" (fun a b c -> (a, b), c)

let parse = function
  | template :: "" :: rules ->
    List.of_seq @@ String.to_seq template, List.map parse_rule rules
  | _ -> assert false

type pairtbl = ((char * char), int ref) Hashtbl.t

(* increase the value of tbl[k] by amount. Add new key if needed. *)
let inc tbl k amount : unit =
  match Hashtbl.find_opt tbl k with
  | None -> Hashtbl.add tbl k (ref amount)
  | Some c -> c := !c + amount

let insert_pair rules polymap : pairtbl =
  let keys (a, b) x = (a, x), (x, b) in
  let updates = ref [] in
  polymap |> Hashtbl.iter (fun pair count ->
      match List.assoc_opt pair rules with
      | None -> ()
      | Some x ->
        let amount = !count and ka, kb = keys pair x in
        count := 0; (* pair no longer exists *)
        updates := (ka, amount) :: (kb, amount) :: !updates);
  (* apply updates *)
  List.iter (fun (k, a) -> inc polymap k a) !updates;
  polymap

(* to pairtbl *)
let to_tbl poly : pairtbl =
  let map = Hashtbl.create 5 in
  let rec aux = function
    | [] | [_] -> ()
    | a::b::xs ->
      Hashtbl.add map (a, b) (ref 1);
      aux (b::xs)
  in
  aux poly; map

(* collect chars from pairtbl *)
let collect_char polymap =
  let tbl = Hashtbl.create 5 in
  Hashtbl.iter (fun (a, b) c -> inc tbl a !c; inc tbl b !c) polymap;
  Hashtbl.to_seq tbl
  |> Seq.map (fun (k, c) -> k, !c)
  |> List.of_seq

(* solve and print answer *)
let solve poly rules n : unit =
  let firstchar = List.hd poly and lastchar = last poly in
  let polymap = to_tbl poly |> repeat (insert_pair rules) n in
  collect_char polymap
  |> List.map (fun (k, n) ->
      (* callibrate *)
      if k = firstchar || k = lastchar then (n+1)/2 else n/2)
  |> List.sort Int.compare
  |> (fun xs ->
      let least = List.hd xs and most = last xs in
      most - least)
  |> Printf.printf "%d\n"

let () =
  let template, rules = IO.read_lines () |> parse in
  let solve = solve template rules in
  begin
    (* PART 1 *) solve 10;
    (* PART 2 *) solve 40;
  end
