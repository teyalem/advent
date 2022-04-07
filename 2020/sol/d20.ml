open Ut

(* Monochrome Pixel *)
module Pixel = struct
  type t = Black | White | Monster (* "#", "." "O" respectively *)

  let default = White

  let of_char = function
    | '#' -> Black
    | '.' -> White
    | 'O' -> Monster
    | _ -> assert false

  let to_char = function
    | Black -> '#'
    | White -> '.'
    | Monster -> 'O'

end

(* Square Image Block from satellite *)
module Image = struct

  include Block.Make(Pixel)

  type block_list = (int * t) list

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

  (* Return edges of a block *)
  let get_edges block =
    let init = Array.init in
    let dim = dimx block in
    [ init dim (fun x -> get block x 0);
      init dim (fun x -> get block x (dim-1));
      init dim (fun y -> get block 0 y);
      init dim (fun y -> get block (dim-1) y) ]

  (* solve jigsaw puzzle of images by making a graph *)
  let assemble (blocks: block_list) : graph =
    let open List in
    (* predicate: test a and b is the same edge *)
    let rev_array a = Array.(to_list a |> List.rev |> of_list) in
    let is_same_edge a b = a = b || a = rev_array b in

    (* map block association list to edge list *)
    let all_edges = blocks |> map (fun (i, b) -> i, get_edges b) in

    (* find adjacent block of specific edge *)
    let find_adj_blocks block_idx edge =
      find_opt
        (fun (idx, edges) ->
           idx <> block_idx && exists (is_same_edge edge) edges)
        all_edges
      |> Option.map fst
    in

    (* edge list to adjacent list *)
    all_edges |> map (fun (i, edges) ->
         let adjs = filter_map (find_adj_blocks i) edges in
         i, adjs)

  (* remove border from an image block *)
  let remove_border (block: t) : t =
    sub block (1, 1) (dimx block - 2, dimy block - 2)

  (* return block rotated 90-degree counter-clockwise *)
  let rotate block =
    let len = dimx block in
    let mat = make len len in
    iteri (fun x y c -> set mat (len-1 - y) x c) block;
    mat

  (* flip block horizontally *)
  let flip_horiz block =
    let len = dimx block in
    let mat = make len len in
    iteri (fun x y c -> set mat x (len-1 - y) c) block;
    mat

  (* flip block vertically *)
  let flip_vert block = 
    let len = dimx block in
    let mat = make len len in
    iteri (fun x y c -> set mat (len-1 - x) y c) block;
    mat

  (* transpose block *)
  let transpose block =
    let len = dimx block in
    let mat = make len len in
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
    let a = to_matrix a
    and b = to_matrix b in
    let len = Array.length a in
    let side_a_right = a.(len-1)
    and side_b_left = b.(0)
    in side_a_right = side_b_left

  let is_matched_ud a b =
    let a = to_matrix a
    and b = to_matrix b in
    let len = Array.length a in
    let side_a_down = Array.map (fun arr -> arr.(len-1)) a
    and side_b_upper = Array.map (fun arr -> arr.(0)) b
    in side_a_down = side_b_upper

  (* construct full image by traversing graph *)
  let construct (blocks: block_list) (graph: graph) : t =
    let open List in

    (* generate array of blocks. row, column order *)
    let block_array = Graph.unfold_lattice graph
            |> map (fun l -> l |> map (fun i -> assoc i blocks))
            |> map Array.of_list
            |> Array.of_list
    in

    (* print_endline "Block Array Created"; (* debug *) *)

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

    let frame = Array.init len (fun _ -> Array.init len (fun _ -> make 0 0)) in
    let images = loop 0 frame |> Option.get in

    (* print_endline "Image Array Found"; (* debug *) *)

    let elemwise_concat a b =
      map2 (fun a b -> Array.append a b) a b
    in

    (* Concatencate images *)
    Array.to_list images
    |> map (fun l ->
        Array.to_list l
        |> map (fun b -> remove_border b
                         |> transpose
                         |> to_matrix
                         |> Array.to_list) (* pixel array list list *)
        |> fun l -> fold_left (fun p n -> elemwise_concat p n) (hd l) (tl l)
                    |> Array.of_list)
    |> Array.concat
    |> of_matrix

end

(* a picture of monster *)
let monster = [
  "                  # ";
  "#    ##    ##    ###";
  " #  #  #  #  #  #   "
]

(* convert a picture to match list *)
let to_matchlist (m: string list) : (int * int * Pixel.t) list =
  m |> List.mapi (fun y l ->
      String.to_seq l
      |> List.of_seq
      |> List.mapi (fun x c -> (x, y, c))
      |> List.filter (fun (_, _, c) -> c <> ' ')
      |> List.map (fun (x, y, c) -> x, y, Pixel.of_char c))
  |> List.concat

(* mark monster on imageblock mat *)
let mark_matches m mat =
  let m_width = List.hd m |> String.length
  and m_height = List.length m
  and mat_width = Image.dimx mat
  and mat_height = Image.dimy mat
  and matchlist = to_matchlist m in

  let found = ref false in

  for y = 0 to mat_height - m_height - 1 do
    for x = 0 to mat_width - m_width - 1 do
      if List.for_all (fun (dx, dy, c) -> Image.get mat (x+dx) (y+dy) = c) matchlist
      then begin (* found *)
        found := true;
        List.iter (fun (dx, dy, _) -> Image.set mat (x+dx) (y+dy) Pixel.Monster) matchlist;
      end
      else ()
    done
  done;
  !found

let main path =
  let data = open_in path |> IO.input_all |> Image.parse_blocks in
  begin
    (* PART 1 *)
    let graph = Image.assemble data in

    graph
    |> List.filter (fun (_, e) -> List.length e = 2)
    |> List.map fst
    |> List.fold_left Int.mul 1
    |> print_int;

    print_newline ();

    (* PART 2 *)
    let image = Image.construct data graph in

    let monster_image =
      Image.all_mutations image
      |> List.find (mark_matches monster)
    in

    monster_image
    |> Image.count_occur Pixel.Black
    |> print_int;

    print_newline ();
  end

let _ = Arg.parse [] main ""
