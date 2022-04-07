open Ut

let neighs_delta = [ 0, -1; 0, 1; -1, 0; 1, 0 ]
let neigh pos =
  List.map (fun dpos -> Coord.add pos dpos) neighs_delta

module Tile = struct
  type t = char

  let default = '.'

  let to_char c = c
  let of_char c = c
end

module Map = struct
  include Block.Make(Tile)

  let get_cell m (x, y) =
    try get m x y
    with _ -> Tile.default

  let alignment_parameters map = 
    let inters = ref [] in
    iteri (fun x y c ->
        if c = '#' && List.for_all (fun pos -> get_cell map pos = '#') @@ neigh (x, y)
        then inters := (x, y) :: !inters
        else ()) map;
    List.map (fun (x, y) -> x*y) !inters

  let find_bot map =
    let rpos = ref (0, 0) in
    iteri (fun x y c -> if c = '^' then rpos := (x, y) else ()) map;
    !rpos

  let find_path map =
    let pos = find_bot map in
    let rec move prev now =
      let scaffold =
        List.map (fun p -> p, get_cell map p) @@ neigh now
        |> List.filter_map (fun (p, t) -> if t = '#' && p <> prev then Some p else None)
      in
      match List.length scaffold with
        0 -> [now]
      | 1 -> now :: move now (List.hd scaffold)
      | _ -> (* cross *)
        let next = Coord.(add now (sub now prev)) in
        now :: move now next
    in
    move (0, 0) pos

end

let to_dir = function
    0, -1 -> 0
  | 1, 0 -> 1
  | 0, 1 -> 2
  | -1, 0 -> 3
  | (x, y) -> failwith @@ Printf.sprintf "pos: %d %d" x y

let diff a b =
  if a = 3 && b = 0
  then 1
  else if a = 0 && b = 3
  then -1
  else b - a

let path_to_coms path =
  List.fold_left (fun (p, d, coms) n ->
      let dir = to_dir @@ Coord.sub n p in
      match diff d dir with
       -1 -> n, dir, "F"::"L"::coms
      | 0 -> n, dir, "F"::coms
      | 1 -> n, dir, "F"::"R"::coms
      | _ -> assert false)
    (List.hd path, 0, []) (* up, right, down, left *)
    (List.tl path)
  |> (fun (_, _, coms) -> coms)

  |> List.fold_left (fun (p, coms) n ->
      match n with
        "L" -> n, ("L", 1)::coms
      | "R" -> n, ("R", 1)::coms

      | "F" ->
        if p = n
        then let _, i = List.hd coms in n, ("F", i+1)::(List.tl coms)
        else n, ("F", 1)::coms

      | _ -> assert false)
    ("", [])
  |> snd

  |> List.map (fun (c, i) ->
      if c = "F"
      then string_of_int i
      else c)

let make_code map =
  Map.find_path map
  |> path_to_coms
  |> List.iter (Printf.printf " %s");
  print_newline ()

let main path =
  let data = open_in path |> IO.read_file |> IntCode.parse_code in
  begin
    (* PART 1 *)
    let m = IntCode.load data in
    IntCode.run m;
    
    let map =
      Seq.unfold (fun m ->
          if IntCode.is_output_empty m
          then None
          else Some (char_of_int @@ IntCode.get_output m, m)) m
      |> String.of_seq
      |> Delim.split_line
      |> Map.parse
    in

    print_int @@ sum @@ Map.alignment_parameters map;

    print_newline ();

    (* PART 2 *)
    let m = IntCode.load data in
    IntCode.set m 0 2; (* wake up vaccum machine *)
    IntCode.run m;

    let main = "A,B,A,C,B,C,B,C,A,C\n" in
    let a = "R,12,L,6,R,12\n"
    and b = "L,8,L,6,L,10\n"
    and c = "R,12,L,10,L,6,R,10\n" in
    let feed = "n\n" in

    [ main; a; b; c; feed ]
    |> List.iter (fun s ->
        s |> String.iter (fun c ->
            IntCode.print_output m;
            print_char c;
            IntCode.set_input m @@ int_of_char c;
            IntCode.run m));

    let dust = ref 0 in
    try while not @@ IntCode.is_output_empty m do
      let c = IntCode.get_output m in
      if c < 127
      then print_char @@ char_of_int c
      else begin
        dust := c;
        raise Exit
      end
    done with Exit -> Printf.printf "%d\n" !dust

  end

let () = Arg.parse [] main ""
