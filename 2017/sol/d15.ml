open Ut

let parse str =
  Scanf.sscanf str "Generator %s starts with %d" (fun g n -> g, n)

let gen mul den init =
  Seq.iterate (fun n -> (n * mul) mod den) init
  |> Seq.drop 1

let b16 = 1 lsl 16 - 1

let same_low_bits (a, b) =
  a land b16 = b land b16

let divide_by n m =
  m mod n = 0

let gen_a = gen 16807 2147483647
let gen_b = gen 48271 2147483647

let judge n gens =
  gens
  |> Seq.take n
  |> Seq.filter same_low_bits
  |> Seq.length

let () =
  let data = IO.read_lines () |> List.map parse in
  (* let data = [ "A", 65; "B", 8921] in *)
  let a, b =
    List.(assoc "A" data, assoc "B" data)
    |> (fun (a, b) -> gen_a a, gen_b b)
  in
  (* PART 1 *)
  Seq.zip a b
  |> judge 40_000_000
  |> Printf.printf "%d\n";

  (* PART 2 *)
  Seq.(filter (divide_by 4) a, filter (divide_by 8) b)
  |> (fun (a, b) -> Seq.zip a b)
  |> judge 5_000_000
  |> Printf.printf "%d\n";
