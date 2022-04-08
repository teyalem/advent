open Ut

let parse str =
  let sw = String.starts_with in
  let sc = Scanf.sscanf in
  if sw ~prefix: "rect" str then
    sc str "rect %dx%d" (fun a b -> "rect", a, b)
  else if sw ~prefix: "rotate" str then
    sc str "rotate %s %s@=%d by %d" (fun m _ a b -> m, a, b)
  else assert false

let do_inst scr (m, a, b) =
  (match m with
  | "rect" ->
    for x = 0 to a-1 do
      for y = 0 to b-1 do
        Mat.set scr x y '#'
      done
    done
  | "row" ->
    Mat.get_row scr a
    |> Array.iteri (fun x v -> Mat.set scr ((x+b) mod Mat.dimx scr) a v)
  | "column" ->
    Mat.get_col scr a
    |> Array.iteri (fun y v -> Mat.set scr a ((y+b) mod Mat.dimy scr) v)
  | _ -> Printf.eprintf "unknown mode: %s\n" m; assert false);
  scr

let () =
  let data = IO.read_lines () |> List.map parse in
  begin
    (* PART 1 *)
    let scr = Mat.make 50 6 '.' in
    List.fold_left do_inst scr data
    |> Mat.fold (fun c v -> c + if v = '#' then 1 else 0) 0
    |> Printf.printf "%d\n";

    (* PART 2 *)
    Mat.iter_row (Printf.printf "%c") (fun () -> Printf.printf "\n") scr;
  end
