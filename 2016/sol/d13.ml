open Ut

let favnum = 1352

let count_binary_ones n =
  n
  |> Seq.unfold (fun n ->
      if n = 0 then None else Some (n mod 2, n/2))
  |> Seq.fold_left Int.add 0

let cell x y =
  x*x + 3*x + 2*x*y + y + y*y + favnum
  |> count_binary_ones
  |> (fun n -> if n mod 2 = 0 then '.' else '#')

module Maze = struct
  type space = unit
  type state = int * int
  type data = int

  let data_id = 0

  let is_end _ ((x, y), _) =
    x = 31 && y = 39

  let neighbors _ ((x, y), d) =
    Neigh.(neighbors von_neumann (x, y))
    |> List.filter (fun (x, y) -> x >= 0 && y >= 0 && cell x y = '.')
    |> List.map (fun p -> p, d+1)
end

module IP = struct
  type t = int * int
  let compare = Stdlib.compare
end

let solve1 () =
  Pathfind.bfs (module IP) (module Maze) ~start: (1, 1) ()

let solve2 () =
  let visited = Hashtbl.create 100 in
  let queue = Queue.create () in
  let i = ref 0 in
  let rec aux () =
    if Queue.is_empty queue then ()
    else begin
      let p, d = Queue.pop queue in
      if not @@ Hashtbl.mem visited p then begin
        Hashtbl.replace visited p true;
        incr i;
        Maze.neighbors () (p, d)
        |> List.filter (fun (_, d) -> d <= 50)
        |> List.iter (fun st -> Queue.push st queue)
      end;
      aux ()
    end
  in
  Queue.push ((1, 1), 0) queue;
  aux ();
  !i

let () =
  begin
    (* PART 1 *)
    solve1 () |> Printf.printf "%d\n";

    (* PART 2 *)
    solve2 () |> Printf.printf "%d\n";
  end
