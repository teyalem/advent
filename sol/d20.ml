open Ut

module Tile = struct
  type t = Empty | Floor | Wall | Portal of string

  let default = Empty

  let to_char _ = assert false
  let of_char c =
    match c with
      ' ' -> Empty
    | '.' -> Floor
    | '#' -> Wall
    | c -> Portal (String.make 1 c)

  let portal_name c =
    match c with
      Portal c -> c
    | _ -> assert false

  let is_portal c =
    match c with
      Portal _ -> true
    | _ -> false
end

module Map = struct
  include Block.Make(Tile)

  let get_cell map (x, y) =
    try Some (get map x y)
    with _ -> Some Tile.default

  let set_cell map (x, y) c = set map x y c

  let set_portals map =
    let set_portal_vert first x y =
      let second = Tile.portal_name @@ get map x (y+1) in
      if get_cell map (x, (y-1)) = Some Tile.Floor
      then begin
        set map x y (Tile.Portal (first ^ second));
        set map x (y+1) Tile.Empty
      end
      else begin
        set map x y Tile.Empty;
        set map x (y+1) (Tile.Portal (first ^ second))
      end

    and set_portal_horiz first x y =
      let second = Tile.portal_name @@ get map (x+1) y in
      if get_cell map (x-1, y) = Some Tile.Floor
      then begin
        set map x y (Tile.Portal (first ^ second));
        set map (x+1) y Tile.Empty
      end
      else begin
        set map x y Tile.Empty;
        set map (x+1) y (Tile.Portal (first ^ second))
      end

    in

    iteri (fun x y c ->
        match c with
          Tile.Portal c ->
          if String.length c = 1
          then begin
            if Tile.is_portal @@ get map (x+1) y
            then set_portal_horiz c x y
            else set_portal_vert c x y
          end
          else ()

        | _ -> ())
      map;
    map

  let collect_dests map start =
    let q = Queue.create () in
    let rec bfs () =
      if Queue.is_empty q
      then []
      else
        let pos, dis = Queue.pop q in
        let t = get_cell map pos |> Option.get in

      if Tile.is_portal t
      then (pos, Tile.portal_name t, dis) :: bfs ()
      else if t = Tile.Wall
      then bfs ()

      else begin
        set_cell map pos Tile.Wall;
        List.map (Coord.add pos) [ 0, -1; 1, 0; 0, 1; -1, 0 ]
        |> List.filter (fun pos ->
            let t = get_cell map pos |> Option.get in
            t = Tile.Wall || Tile.is_portal t)
        |> List.iter (fun pos -> Queue.push (pos, dis+1) q);
        bfs ()
      end
  in

  set_cell map start Tile.Floor;
  Queue.push (start, 0) q;
  bfs ()

  let find_portals map =
    let result = ref [] in
    iteri (fun x y c ->
        match c with
          Portal c -> result := (x, y, c) :: !result
        | _ -> ())
      map;
    !result

  let make_graph map =
    find_portals map
    |> List.map (fun (x, y, c) ->
        c, collect_dests (copy map) (x, y))

end

let main path =
  let data = open_in path |> IO.read_lines |> Map.parse |> Map.set_portals in
  begin

    (* PART 1 *)
    Map.make_graph data
  |> List.iter (fun (node, edges) ->
        Printf.printf "%s:" node;
        List.iter (fun (_, t, _) -> Printf.printf " %s" t) edges;
        print_newline ())
  end

let () = Arg.parse [] main ""
