open Ut

let parse_repl str =
  Scanf.sscanf str "%s => %s" (fun a b -> a, b)

let parse ss =
  let repl = Seq.take_while ((<>) "") ss
             |> Seq.map parse_repl
             |> List.of_seq
  and mol = Seq.drop_while ((<>) "") ss |> Seq.nth 1 |> Option.get in
  repl, mol

let is_lowercase c =
  'a' <= c && c <= 'z'

let split_mol mol =
  let aux seq =
    Seq.uncons seq
    |> Option.map (fun (h, t) ->
        let head = Seq.take_while is_lowercase t
        and tail = Seq.drop_while is_lowercase t in
        String.of_seq (Seq.cons h head), tail)
  in
  Seq.unfold aux @@ String.to_seq mol
  |> List.of_seq

let create_mocules repl mol =
  let mol = split_mol mol in
  let mols = ref [] in
  let rec aux left = function
    | [] -> ()
    | x::xs ->
      List.filter (fun (k, _) -> k = x) repl
      |> List.map snd
      |> List.map (fun r -> List.rev_append left (r::xs))
      |> List.iter (fun m -> mols := m :: !mols);
      aux (x::left) xs
  in
  aux [] mol;
  List.map (String.concat "") !mols

(* source: https://www.reddit.com/r/adventofcode/comments/3xflz8/comment/cy4etju/?utm_source=share&utm_medium=web2x&context=3
 * *)
let part2 mol =
  let mol = split_mol mol in
  let len = List.length mol
  and rnars = List.length
    @@ List.filter (fun x -> x = "Rn" || x = "Ar") mol
  and ys = List.length @@ List.filter ((=) "Y") mol in
  len - rnars - 2*ys - 1

let () =
  let repl, mol = IO.read_lines () |> List.to_seq |> parse in
  begin
    (* PART 1 *)
    create_mocules repl mol
    |> List.sort_uniq String.compare
    |> List.length
    |> Printf.printf "%d\n";

    (* PART 2 *)
    part2 mol |> Printf.printf "%d\n";
  end
