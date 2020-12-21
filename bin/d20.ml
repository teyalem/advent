open Advent

(* Graph *)
module Graph = struct
  type t = (int * int list) list

  (* debug: print graph *)
  let print g =
    List.iter (fun (i, e) ->
        Printf.printf "%d:" i;
        List.iter (Printf.printf " %d") e;
        print_newline ()) g

  (* debug: print graph in graphviz format
   * WARNING: output is very messy. *)
  let print_graphviz g =
    print_endline "digraph G {";
    List.iter (fun (i, e) ->
        List.iter (Printf.printf "%d -> %d\n" i) e) g;
    print_endline "}"

  (* check node is connected to other node *)
  let is_connected node other_node graph =
    graph
    |> List.assoc node
    |> List.exists (fun n -> n = other_node)

  (* count number of edge *)
  let num_edge node graph =
    List.assoc node graph |> List.length

  (* remove node from a graph *)
  let remove node graph =
    graph
    |> List.remove_assoc node
    |> List.map (fun (i, e) -> i, List.filter (fun n -> n <> node) e)

  let find_inner node graph =
    match List.assoc node graph with
    | [left; right] ->
      List.assoc left graph
      |> List.filter (fun pin -> is_connected pin right graph)
      |> List.find (fun n -> node <> n)

    | _ -> assert false

  let rec follow_graph node graph =
    let edge = List.assoc node graph
    and graph = remove node graph in
    match edge with
    | [] -> [ node ]
    | [ next ] -> node :: (follow_graph next graph)
    | _ -> assert false

  let traverse node graph =
    let rec traverse_row node pin graph =
      match List.assoc node graph with
      | [_] -> (* Terminal node *)
        let graph = remove node graph in
        [node], graph

      | [left; right] ->
        let next_node = if left = pin then right else left in
        let next_pin = find_inner node graph in
        let graph = remove node graph in
        let ns, graph = traverse_row next_node next_pin graph in
        node::ns, graph

      | _ -> assert false
    in

    let rec traverse_column node pin graph =
      match List.assoc node graph with
      | [_] -> [follow_graph node graph] (* a single line *)
      | [left; right] ->
        let next_node, e = if left = pin || pin = 0 then right, left else left, right in
        let next_pin = find_inner node graph in
        let graph = remove node graph in
        let row, graph = traverse_row e next_pin graph in
        (node::row) :: traverse_column next_node next_pin graph

      | _ -> assert false

    in

    traverse_column node 0 graph

end

(* Monochrome Pixel *)
module Pixel = struct
  type t = Black | White | Monster (* "#", "." "O" respectively *)

  let white = White
  let black = Black
  let monster = Monster

  let of_char = function
    | '#' -> Black
    | '.' -> White
    | 'O' -> Monster
    | _ -> assert false

  let to_char = function
    | Black -> '#'
    | White -> '.'
    | Monster -> 'O'

  let print p = to_char p |> print_char

end

(* Square Image Block from satellite *)
module ImageBlock = struct

  type pixel = Pixel.t
  type t = pixel array array (* pixel matrix *)
  type block_list = (int * t) list

  (* make empty block *)
  let make dim = Array.make_matrix dim dim Pixel.white

  (* get/set a position of block *)
  let get block x y = block.(x).(y)
  let set block x y n = block.(x).(y) <- n

  (* iterate through block *)
  let iteri f block =
    for x = 0 to Array.length block - 1 do
      for y = 0 to Array.length block.(0) - 1 do
        f x y (get block x y)
      done
    done

  (* debug: print block *)
  let print block =
    block |> Array.(iter (fun arr ->
        iter Pixel.print arr;
        print_newline ()));
    print_newline ()

  (* return original monochrome image *)
  let to_raw block =
    block |> Array.map (fun arr ->
        Array.map Pixel.to_char arr)

  (* parse a matrix *)
  let parse ss =
    (* converting string list to char list list *)
    let ss = List.map (fun s -> String.to_seq s |> List.of_seq) ss in
    let dim = List.length ss in
    let mat = make dim in
    ss |> List.iteri
      (fun y s ->
         s |> List.iteri
           (fun x c -> set mat x y (Pixel.of_char c)));
    mat

  (* parse a block with number *)
  let parse_with_number str =
    match Delim.split_line str with
    | [] -> assert false
    | num :: image ->
      let num = Scanf.sscanf num "Tile %d:" (fun n -> n)
      and image = parse image
      in num, image

  (* parse array of blocks *)
  let parse_blocks str =
    Delim.split "\n\n" str
    |> List.map parse_with_number

  let get_edges block =
    let width = Array.length block
    and height = Array.length block.(0) in

    let up = Array.init width (fun x -> get block x 0)
    and down = Array.init width (fun x -> get block x (height-1))
    and left = Array.init height (fun y -> get block 0 y)
    and right = Array.init height (fun y -> get block (width-1) y)
    in
    [up; down; left; right]

  (* reverse array *)
  let rev_array arr =
    Array.to_list arr
    |> List.rev
    |> Array.of_list

  (* test a and b is the same edge *)
  let is_same_edge a b = a = b || a = rev_array b

  (* find adjacent block of specific edge *)
  let find_adj_block i edge all_edges =
    let all_edges = List.remove_assoc i all_edges in
    all_edges
    |> List.find_opt
      (fun (_, edges) ->
         List.exists (is_same_edge edge) edges)

  (* solve jigsaw puzzle of images by making a graph *)
  let assemble (blocks: block_list) : Graph.t =
    let open List in
    (* block list to edge list *)
    let all_edges : (int * pixel array list) list = blocks |> map (fun (i, b) -> i, get_edges b) in
    (* edge list to adjacent list *)
    all_edges
    |> map (fun (i, edges) ->
        let idxs: int list =
          edges
          |> filter_map (fun edge -> find_adj_block i edge all_edges)
          |> map fst
        in i, idxs)

  (* remove border from an image block *)
  let remove_border (block: t) : t =
    let open Array in
    let len = length block in
    sub block 1 (len-2)
    |> map (fun a -> sub a 1 (len-2))

  (* return block rotated 90-degree counter-clockwise *)
  let rotate block =
    let len = Array.length block in
    let mat = make len in
    iteri (fun x y c -> set mat (len-1 - y) x c) block;
    mat

  (* flip block horizontally *)
  let flip_horiz block =
    let len = Array.length block in
    let mat = make len in
    iteri (fun x y c -> set mat x (len-1 - y) c) block;
    mat

  (* flip block vertically *)
  let flip_vert block = 
    let len = Array.length block in
    let mat = make len in
    iteri (fun x y c -> set mat (len-1 - x) y c) block;
    mat

  (* transpose block *)
  let transpose block =
    let len = Array.length block in
    let mat = make len in
    iteri (fun x y c -> set mat y x c) block;
    mat

  (* return all mutations of a block *)
  let all_mutations block =
    let rec multi_mutate f c l =
      if c = 0 then l
      else
        let h = List.hd l
        and al = List.map f l
        in multi_mutate f (c-1) (h::al)
    in
    let rotations = multi_mutate rotate 3 [block] in
    List.concat [
      rotations;
      List.map flip_horiz rotations;
      List.map flip_vert rotations; ]

  let is_matched_lr a b =
    let len = Array.length a in
    let side_a_right = a.(len-1)
    and side_b_left = b.(0)
    in side_a_right = side_b_left

  let is_matched_ud a b =
    let len = Array.length a in
    let side_a_down = Array.map (fun arr -> arr.(len-1)) a
    and side_b_upper = Array.map (fun arr -> arr.(0)) b
    in side_a_down = side_b_upper

  (* construct full image by traversing graph *)
  let construct (blocks: block_list) (graph: Graph.t) : t =
    let open List in

    (* first corner of array *)
    let first_corner = graph |> filter (fun (_, e) -> length e = 2) |> hd |> fst in

    (* generate array of blocks. row, column order *)
    let block_array = Graph.traverse first_corner graph
            |> map (fun l -> l |> map (fun i -> assoc i blocks))
            |> map Array.of_list
            |> Array.of_list
    in
    print_endline "Block Array Created"; (* debug *)

    let len = Array.length block_array in

    let is_matched x y block arr =
      match x, y with
      | 0, 0 -> true
      | x, 0 -> is_matched_lr arr.(0).(x-1) block
      | 0, y -> is_matched_ud arr.(y-1).(0) block
      | x, y -> is_matched_lr arr.(y).(x-1) block && is_matched_ud arr.(y-1).(x) block
    in

    (* Assume correct orientation is only one *)
    let rec loop idx arr =
      if idx >= len*len then Some arr (* succeed *)
      else 
        let x = idx mod len and y = idx / len in
        let tile = block_array.(y).(x) in
        all_mutations tile
        |> filter (fun m -> is_matched x y m arr)
        |> find_map (fun m ->
            let arr = Array.init len (fun i -> Array.copy arr.(i)) in
            arr.(y).(x) <- m;
            loop (idx+1) arr)
    in

    let images = loop 0 (Array.init len (fun _ -> Array.make len [||])) |> Option.get in

    print_endline "Image Array Found"; (* debug *)

    let elemwise_concat a b =
      map2 (fun a b -> Array.append a b) a b
    in

    (* Concatencate images *)
    Array.to_list images
    |> map (fun l ->
        Array.to_list l
        |> map (fun b -> remove_border b |> transpose |> Array.to_list) (* pixel array list list *)
        |> fun l -> fold_left (fun p n -> elemwise_concat p n) (hd l) (tl l)
                    |> Array.of_list)
    |> Array.concat

end

let monster = [
  "                  # ";
  "#    ##    ##    ###";
  " #  #  #  #  #  #   "
]

let to_matchlist (m: string list) : (int * int * Pixel.t) list =
  m |> List.mapi (fun y l ->
      String.to_seq l
      |> List.of_seq
      |> List.mapi (fun x c -> (x, y, c))
      |> List.filter (fun (_, _, c) -> c <> ' ')
      |> List.map (fun (x, y, c) -> x, y, Pixel.of_char c))
  |> List.concat

let mark_matches m mat =
  let m_width = List.hd m |> String.length
  and m_height = List.length m
  and mat_width = Array.length mat.(0)
  and mat_height = Array.length mat
  and matchlist = to_matchlist m in

  let result = ref false in

  for y = 0 to mat_height - m_height - 1 do
    for x = 0 to mat_width - m_width - 1 do
      if List.for_all (fun (dx, dy, c) -> mat.(y+dy).(x+dx) = c) matchlist
      then begin (* found *)
        result := true;
        List.iter (fun (dx, dy, _) -> mat.(y+dy).(x+dx) <- Pixel.monster) matchlist
      end
      else ()
    done
  done;
  !result


let main path =
  let data = open_in path |> IO.read_file |> ImageBlock.parse_blocks in
  begin
    (* PART 1 *)
    let graph = ImageBlock.assemble data in

    graph
    |> List.filter (fun (_, e) -> List.length e = 2)
    |> List.map fst
    |> List.fold_left Int.mul 1
    |> print_int;

    print_newline ();

    (* PART 2 *)
    let image = ImageBlock.construct data graph in

    let monster_image =
      ImageBlock.all_mutations image
      |> List.find (mark_matches monster)
    in

    monster_image
    |> Array.to_list
    |> Array.concat
    |> Array.to_list
    |> List.filter (fun n -> n = Pixel.Black)
    |> List.length
    |> print_int

  end

let _ = Arg.parse [] main ""
