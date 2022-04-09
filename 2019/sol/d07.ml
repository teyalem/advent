open Ut

let rec combi l =
  match l with
  | [] -> []
  | [n] -> [[n]]
  | l ->
    List.concat_map
      (fun n ->
         List.map
           (fun l -> n :: l)
           (combi @@ List.filter (fun i -> i <> n) l))
      l

let prepare data l =
  let ms = List.init 5 (fun _ -> IntCode.load data) in
  List.iter2 (fun m i -> IntCode.(push_input m i; run m)) ms l;
  ms

let run_conf input ms =
  List.fold_left
    (fun prev m ->
       IntCode.(push_input m prev;
                run m;
                pop_output m))
    input
    ms

let rec run_conf_rec input ms =
  let output = run_conf input ms in
  if List.nth ms 4 |> IntCode.is_halt
  then output
  else run_conf_rec output ms

let () =
  let data = IO.read_all () |> IntCode.parse_code in
  begin
    (* PART 1 *)
    combi [0; 1; 2; 3; 4]
    |> List.map (fun l -> prepare data l |> run_conf 0)
    |> List.fold_left max min_int
    |> print_int;

    print_newline ();

    (* PART 2 *)
    combi [5; 6; 7; 8; 9]
    |> List.map (fun l -> prepare data l |> run_conf_rec 0)
    |> List.fold_left max min_int
    |> print_int
  end
