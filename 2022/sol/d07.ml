open Ut

module Path = struct
  type t = string list
  let compare = List.compare String.compare
end

module M = Map.Make(Path)

let is_command = String.starts_with ~prefix: "$"
let is_cd = String.starts_with ~prefix: "$ cd"
let is_ls = String.starts_with ~prefix: "$ ls"

let change_cwd cwd com =
  Scanf.sscanf com "$ cd %s" (function
      | "/" -> []
      | ".." -> List.tl cwd
      | d -> d::cwd)

let construct_dirtree =
  let rec read_ls cwd tree = function
    | [] -> tree, []
    | s::ss ->
      if is_command s then tree, (s::ss)
      else
        let p =
          Scanf.sscanf s "%s %s" (fun size file ->
            if size = "dir" then -1, file else int_of_string size, file)
        in
        let tree =
          tree |> M.update cwd
            (function None -> Some [p] | Some xs -> Some (p::xs))
        in
        read_ls cwd tree ss
  in

  let rec read_sh cwd tree = function
    | [] -> tree
    | s::ss ->
      if is_cd s then
        read_sh (change_cwd cwd s) tree ss
      else if is_ls s then
        let tree, ss = read_ls cwd tree ss in
        read_sh cwd tree ss
      else
        assert false
  in
  read_sh [] M.empty

let du_all tree =
  let is_dir s = s < 0 in
  let rec scan du cwd =
    let ts, du =
      M.find cwd tree
      |> List.fold_left (fun (ts, du) (s, f) ->
          if is_dir s then
            let s, du = scan du (f::cwd) in
            ts + s, du
          else ts + s, du)
        (0, du)
    in
    ts, M.add cwd ts du
  in
  scan M.empty []

(*
let print_dir tree =
  let open Printf in
  tree
  |> M.iter (fun wd ls ->
      if wd = [] then printf "/" else List.rev wd |> List.iter (printf "/%s");
      printf "\n";
      List.iter (fun (s, f) -> printf "%d %s\n" s f) ls)
*)

let disk_space = 70_000_000
let space_required = 30_000_000

let () =
  let data = open_in Sys.argv.(1) |> IO.input_lines in
  let tree = construct_dirtree data in
  let root_size, size = du_all tree in
  (* PART 1 *)
  M.to_seq size
  |> Seq.map snd
  |> Seq.filter (fun s -> s <= 100_000)
  |> Seq.fold_left (+) 0
  |> print_int;
  print_newline ();

  (* PART 2 *)
  let space_needed = root_size + space_required - disk_space in
  M.to_seq size
  |> Seq.map snd
  |> Seq.filter (fun s -> s > space_needed)
  |> List.of_seq
  |> List.sort Int.compare
  |> List.hd
  |> print_int
