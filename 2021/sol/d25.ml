open Ut

module C = struct
  type t =
    | East
    | South
    | Empty

  let default = Empty
  let of_char = function
    | '.' -> Empty
    | 'v' -> South
    | '>' -> East
    | _ -> assert false

  let to_char = function
    | Empty -> '.'
    | South -> 'v'
    | East -> '>'
end

module B = Block.Make(C)

let step b : B.t =
  let nb = B.copy b in
  let move e dx dy =
    let updates = ref [] in
    nb |> B.iteri (fun x y c ->
        let nx = (x+dx) mod B.dimx nb
        and ny = (y+dy) mod B.dimy nb in
        if c = e && B.get nb nx ny = C.Empty then
          updates := (x, y, nx, ny, e) :: !updates);
    !updates |> List.iter (fun (x, y, nx, ny, e) ->
        B.set nb nx ny e;
        B.set nb x y C.Empty)
  in
  move C.East 1 0;
  move C.South 0 1;
  nb

let until_stop b =
  let rec aux i b =
    let nb = step b in
    if b = nb then i+1
    else aux (i+1) nb
  in
  aux 0 b

let () =
  let data = IO.read_lines () |> B.parse in
  begin
    (* PART 1 *)
    until_stop data |> Printf.printf "%d\n";

    (* and it's done. Merry Christmas! *)
  end
