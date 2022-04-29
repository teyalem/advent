open Ut

let parse str =
  Scanf.sscanf str "%s %s %d if %s %s %d" (fun r c n r2 op n2 ->
      (r, c, n, r2, op, n2))

let compop = function
  | "==" -> (=)
  | "!=" -> (<>)
  | ">" -> (>)
  | "<" -> (<)
  | ">=" -> (>=)
  | "<=" -> (<=)
  | _ -> assert false

let iop = function
  | "inc" -> (+)
  | "dec" -> (-)
  | _ -> assert false

module M = Map.Make(String)

let get m r =
  M.find_opt r m
  |> Option.value ~default: 0

let set m r v = M.add r v m

let apply m (r, c, n, r2, op, n2) =
  if (compop op) (get m r2) n2
  then set m r @@ (iop c) (get m r) n
  else m

let find_max_value m =
  M.fold (fun _ a b -> max a b) m min_int

let all_states insts =
  List.to_seq insts
  |> Seq.scan apply M.empty

let () =
  let data = IO.read_lines () |> List.map parse in
  (* PART 1 *)
  List.fold_left apply M.empty data
  |> find_max_value
  |> Printf.printf "%d\n";
  (* PART 2 *)
  all_states data
  |> Seq.map find_max_value
  |> Seq.fold_left max min_int
  |> Printf.printf "%d\n";
