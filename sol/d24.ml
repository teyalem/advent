open Ut
open List

(* NOTE: messy, slow codes *)

(* combinations that satisfies sum(S) = rest. *)
let rec nums rest = function
  | [] -> []
  | n::ns ->
    if n < rest then
      List.map (fun ns -> n::ns) (nums (rest - n) ns)
      @ nums rest ns
    else if n = rest then
      [n] :: nums rest ns
    else
      []

module S = Set.Make(Int)

let print_set ns =
  Printf.printf "[";
  S.iter (Printf.printf " %d") ns;
  Printf.printf " ]\n"

let qenatngle ns = S.fold Int.mul ns 1

let pack_triples eqn lpn ns ps =
  let lps = filter (fun xs -> S.cardinal xs = lpn) ps in
  let aux p1 =
    nums eqn (S.elements @@ S.diff ns p1)
    |> map S.of_list
    |> exists (fun p2 ->
        let p3 = S.diff ns @@ S.union p1 p2 in
        S.fold (+) p3 0 = eqn)
  in
  filter aux lps

let least_qe_part1 ns =
  let eqn = fold_left (+) 0 ns / 3 in
  let packages = nums eqn ns |> map S.of_list in
  let lpn = map S.cardinal packages |> fold_left min max_int in
  let group1s =
    pack_triples eqn lpn (S.of_list ns) packages
  in
  let lgn = map S.cardinal group1s |> fold_left min max_int in
  group1s
  |> filter (fun s -> S.cardinal s = lgn)
  |> map qenatngle
  |> fold_left min max_int

let pack_quadraples eqn lpn ns ps =
  let lps = filter (fun xs -> S.cardinal xs = lpn) ps in
  let aux p1 =
    nums eqn (S.elements @@ S.diff ns p1)
    |> map S.of_list
    |> List.to_seq
    |> Seq.concat_map (fun p2 ->
        nums eqn (S.elements @@ S.diff ns @@ S.union p1 p2)
        |> map S.of_list
        |> map (fun p3 -> p2, p3)
        |> List.to_seq)
    |> Seq.exists (fun (p2, p3) ->
        let p4 = S.diff ns @@ S.union p1 @@ S.union p2 p3 in
        S.fold (+) p4 0 = eqn)
  in
  filter aux lps

let least_qe_part2 ns =
  let eqn = fold_left (+) 0 ns / 4 in
  let packages = nums eqn ns |> map S.of_list in
  let lpn = map S.cardinal packages |> fold_left min max_int in
  let group1s =
    pack_quadraples eqn lpn (S.of_list ns) packages
  in
  let lgn = map S.cardinal group1s |> fold_left min max_int in
  group1s
  |> filter (fun s -> S.cardinal s = lgn)
  |> map qenatngle
  |> fold_left min max_int

let () =
  let data =
    IO.read_lines ()
    |> List.map int_of_string
    (* don't know why, but adding this line will break program. *)
    (* |> List.sort (Fun.flip Int.compare) *)
  in
  begin
    (* PART 1 *)
    least_qe_part1 data |> Printf.printf "%d\n";

    (* PART 2 *)
    least_qe_part2 data |> Printf.printf "%d\n";
  end
