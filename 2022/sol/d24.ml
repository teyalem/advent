open Ut

let parse lines =
  let blizzards = ref [] in
  let map =
    lines
    |> List.to_seq
    |> Seq.mapi (fun y str ->
        String.to_seq str
        |> Seq.mapi (fun x c ->
            let b =
              match c with
              | '>' -> Some (1, 0)
              | '<' -> Some (-1, 0)
              | 'v' -> Some (0, 1)
              | '^' -> Some (0, -1)
              | _ -> None
            in
            Option.iter (fun b -> blizzards := ((x, y), b) :: !blizzards) b;
            if c = '#' then '#' else '.')
        |> Array.of_seq)
    |> Array.of_seq
  in
  map, !blizzards

let modulo n m =
  ((n mod m) + m) mod m

let bl_step sx sy ((x, y), (dx, dy as delta)) =
  let f n d s =
    let n = n + d in
    if n = 0 then s - 2
    else if n = s - 1 then 1
    else n
  in
  let x = f x dx sx and y = f y dy sy in
  (x, y), delta

let get_def m x y =
  try m.(y).(x)
  with Invalid_argument _ -> '#'

let bfs map blseq start dest =
  let q = Queue.create () in
  let visited = Hashtbl.create 1000 in
  let rec aux () =
    if Queue.is_empty q then assert false
    else
      let t, pos, blseq = Queue.pop q in
      if pos = dest then t, blseq
      else if Hashtbl.mem visited (pos, t) then aux ()
      else begin (* neighbours *)
        Hashtbl.add visited (pos, t) true;
        let bls, blseq = Seq.uncons blseq |> Option.get in
        let move =
          Neigh.(neighbors von_neumann pos) @ [pos]
          |> List.filter (fun (x, y) ->
              not (get_def map x y = '#' || List.(mem (x, y) @@ map fst bls)))
        in

        move
        |> List.map (fun pos -> t + 1, pos, blseq)
        |> List.iter (fun st -> Queue.add st q);
        aux ()
      end
  in
  Queue.add (0, start, blseq) q;
  aux ()

let () = Printexc.record_backtrace true
let () =
  let map, bls = open_in Sys.argv.(1) |> IO.input_lines |> parse in
  let sx = Array.length map.(0) and sy = Array.length map in
  let blseq =
    Seq.iterate (List.map (bl_step sx sy)) bls
    |> Seq.memoize
    |> Seq.drop 1
  in
  let start = (1, 0) and dest = (sx - 2, sy - 1) in
  let s1, blseq = bfs map blseq start dest in
  let s2, blseq = bfs map blseq dest start in
  let s3, _ = bfs map blseq start dest in
  Printf.printf "%d\n" s1;
  Printf.printf "%d\n" (s1 + s2 + s3)
