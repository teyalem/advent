open Advent

(* Conway Cube *)
module Cube = struct
  type t = Active | Inactive

  let active = Active
  let inactive = Inactive

  let of_char = function
    | '#' -> Active
    | '.' -> Inactive
    | _ -> raise (Invalid_argument "cube_of_char")

  let to_char = function
    | Active -> '#'
    | Inactive -> '.'

  let print c = to_char c |> print_char

  let next num_an = function
    | Active -> begin
        match num_an with 2 | 3 -> Active
        | _ -> Inactive
      end
    | Inactive -> if num_an = 3 then Active else Inactive

end

module PocketSpace = struct
  type cube = Cube.t

  (* Pocket Space with Conway Cubes *)
  type t = {
    space: cube array array array; (* z, x, y *)
    x_size: int;
    y_size: int;
    z_size: int;
  }

  (* make new space with inactive cubes *)
  let make x_size y_size z_size = {
    space = Array.(init z_size (fun _ -> make_matrix x_size y_size Cube.inactive));
    x_size = x_size;
    y_size = y_size;
    z_size = z_size;
  }

  (* get/set the state of position x y z *)
  (* here we will ignore negetive z, because it's same as positive g *)
  let set ps (x, y, z) s = ps.space.(abs z).(x).(y) <- s
  let get ps (x, y, z) = ps.space.(abs z).(x).(y)

  (* debug: print space *)
  let print ps =
    let printf = Printf.printf in
    for z = 0 to ps.z_size - 1 do
      printf "level: %d\n" z;

      for i = 0 to ps.x_size - 1 do
        for j = 0 to ps.y_size - 1 do
          get ps (i, j, z) |> Cube.print
        done;
        print_newline ()
      done;
      print_newline ()
    done

  (* Initialize pocket space with plain. *)
  let initialize (size: int) (plain: cube array array) =
    let ps = make size size (size/2)
    and p_x = Array.length plain
    and p_y = Array.length plain.(0)
    and mid = size / 2
    in
    let sx = mid - p_x/2
    and sy = mid - p_y/2
    in
    for i = 0 to p_x - 1 do
      for j = 0 to p_y - 1 do
        set ps (sx + i, sy + j, 0) plain.(i).(j)
      done
    done;
    ps

  (* count active neighbor cubes at position x, y, z *)
  let count_active_neighs ps (x, y, z) =
    let count = ref 0 in
    for i = x-1 to x+1 do
      for j = y-1 to y+1 do
        for k = z-1 to z+1 do
          count :=
            !count +
            begin match  get ps (i, j, k) with
              | Cube.Active -> if i = x && j = y && k = z then 0 else 1
              | Cube.Inactive -> 0
              | exception Invalid_argument _ -> 0
            end
        done
      done
    done;
    !count

  (* iterate through its contents with index *)
  let iteri (f: int -> int -> int -> cube -> unit) (ps: t) : unit =
    for i = 0 to ps.x_size - 1 do
      for j = 0 to ps.y_size - 1 do
        for k = 0 to ps.z_size - 1 do
          f i j k (get ps (i, j, k))
        done
      done
    done
          

  let step (ps: t) : unit =
    let updates = ref [] in
    (* collect updates *)
    ps |> iteri (fun i j k c ->
          let pos = i, j, k in
          let an = count_active_neighs ps pos in
          let next = Cube.next an c in
          if next = c then () (* ignore if not changed *)
          else updates := (pos, next) :: !updates);
    (* update states *)
    List.iter (fun (pos, n) -> set ps pos n) !updates

  let simulate ps step_num =
    for _ = 1 to step_num do
      step ps
    done

  let count_active ps =
    let count_plain p =
      let c = ref 0 in
      for i = 0 to ps.x_size - 1 do
        for j = 0 to ps.y_size - 1 do
          match p.(i).(j) with
          | Cube.Active -> incr c
          | Cube.Inactive -> ()
        done
      done;
      !c
    in
    let level_0 = count_plain ps.space.(0) in
    let pos_z = Array.(sub ps.space 1 (ps.z_size - 1)
                       |> fold_left (fun c p -> c + count_plain p) 0)
    in
    level_0 + 2*pos_z

end

module PocketSpace4D = struct
  type cube = Cube.t

  (* Pocket Space with Conway Cubes *)
  type t = {
    space: cube array array array array; (* w, z, y, x *)
    x_size: int;
    y_size: int;
    z_size: int;
    w_size: int;
  }

  (* make new space with inactive cubes *)
  let make x_size y_size z_size w_size =
    let open Array in
    {
      space = init w_size
          (fun _ -> init z_size
              (fun _ -> init y_size
                  (fun _ -> make x_size Cube.inactive)));
      x_size = x_size;
      y_size = y_size;
      z_size = z_size;
      w_size = w_size;
    }

  (* get/set the state of position x y z *)
  let set ps (x, y, z, w) s = ps.space.(w).(z).(y).(x) <- s
  let get ps (x, y, z, w) = ps.space.(w).(z).(y).(x)

  (* Initialize pocket space with plain. *)
  let initialize (size: int) (plain: cube array array) =
    let ps = make size size size size
    and p_x = Array.length plain
    and p_y = Array.length plain.(0)
    and mid = size / 2
    in
    let sx = mid - p_x/2
    and sy = mid - p_y/2
    in
    for i = 0 to p_x - 1 do
      for j = 0 to p_y - 1 do
        set ps (sx + i, sy + j, mid, mid) plain.(i).(j)
      done
    done;
    ps

  (* count active neighbor cubes at position x, y, z, w *)
  let count_active_neighs ps (x, y, z, w) =
    let count = ref 0 in
    for i = x-1 to x+1 do
      for j = y-1 to y+1 do
        for k = z-1 to z+1 do
          for l = w-1 to w+1 do
            count :=
              !count +
              begin match  get ps (i, j, k, l) with
                | Cube.Active ->
                  if i = x && j = y && k = z && l = w
                  then 0 else 1
                | Cube.Inactive -> 0
                | exception Invalid_argument _ -> 0
              end
          done
        done
      done
    done;
    !count

  (* iterate through its contents with index *)
  let iteri (f: int -> int -> int -> int -> cube -> unit) (ps: t) : unit =
    for w = 0 to ps.w_size - 1 do
      for z = 0 to ps.z_size - 1 do
        for y = 0 to ps.y_size - 1 do
          for x = 0 to ps.x_size - 1 do
            f x y z w (get ps (x, y, z, w))
          done
        done
      done
    done

  let step (ps: t) : unit =
    let updates = ref [] in
    (* collect updates *)
    ps |> iteri (fun x y z w c ->
          let pos = x, y, z, w in
          let an = count_active_neighs ps pos in
          let next = Cube.next an c in
          if next = c then () (* ignore if not changed *)
          else updates := (pos, next) :: !updates);
    (* update states *)
    List.iter (fun (pos, n) -> set ps pos n) !updates

  let simulate ps step_num =
    for _ = 1 to step_num do
      step ps
    done

  let count_active ps =
    let count = ref 0 in
    ps |> iteri (fun _ _ _ _ c ->
        if c = Cube.active then incr count else ());
    !count

end

(* parse 2d plain from string list *)
let parse_plain sl =
  let plain = Array.make_matrix
      (List.length sl)
      (String.length (List.hd sl)) Cube.inactive
  in
  begin
    sl |> List.iteri
      (fun i s ->
         String.to_seq s |> List.of_seq
         |> List.iteri (fun j c ->
             let c = Cube.of_char c in plain.(i).(j) <- c
           ));
    plain
  end

let main path =
  let data = open_in path |> IO.read_lines |> parse_plain in
  begin
    (* PART 1 *)
    let ps = PocketSpace.initialize 49 data in
    print_endline "Initialization done";

    PocketSpace.simulate ps 6;
    print_endline "Simulation done";
    PocketSpace.count_active ps |> print_int;

    print_newline ();

    (* PART 2 *)
    let ps = PocketSpace4D.initialize 49 data in
    print_endline "Initialization 4D done";

    PocketSpace4D.simulate ps 6;
    print_endline "Simulation done";

    PocketSpace4D.count_active ps |> print_int;
  end

let _ = Arg.parse [] main ""
