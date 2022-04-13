open Ut

module D = struct
  type t = string * string

  let compare (a, b) (c, d) =
    match String.compare a c with
    | 0 -> String.compare b d
    | v -> v
end

module S = Set.Make(D)

let parse str =
  let material r =
    String.split_on_char '-' r
    |> List.hd
  in
  let device str =
    String.to_seq str
    |> Seq.filter (fun c -> Char.(code 'a' <= code c  && code c <= code 'z'))
    |> String.of_seq
  in
  let rec aux = function
    | [] -> []
    | ("a" | "an") :: r :: d :: xs -> (material r, device d) :: aux xs
    | _ :: xs -> aux xs
  in
  String.split_on_char ' ' str
  |> aux
  |> S.of_list

let gen s =
  let seq = S.to_seq s in
  Seq.map_product (fun a b -> S.of_list [a; b]) seq seq
  |> Seq.append (Seq.map S.singleton seq)

module State = struct
  type t = {
    floor : int;
    map : S.t list;
  }

  let make map = {
    floor = 0;
    map;
  }

  let map s = s.map
  let floor s = s.floor

  let neigh state =
    let f hand st = {
      st with
      map = List.mapi
          (fun i s -> if i = st.floor then S.diff s hand else s)
          st.map
    }
    in

    let pickup st =
      List.nth st.map st.floor
      |> gen
      |> Seq.map (fun hand -> hand, f hand st)
    in

    let up_down st =
      let up = st.floor < 3 in
      let down =
        st.floor > 0 &&
        (st.map
         |> List.filteri (fun i _ -> i < st.floor)
         |> List.for_all S.is_empty
         |> not)
      in
      [ up, +1; down, -1 ]
      |> List.filter fst
      |> List.map (fun (_, i) -> { st with floor = st.floor + i })
      |> List.to_seq
    in

    let not_fried set =
      (not @@ S.exists (fun (_, d) -> d = "generator") set)
      || set
         |> S.filter (fun (_, d) -> d = "microchip")
         |> S.for_all (fun (m, _) -> S.mem (m, "generator") set)
    in

    pickup state
    |> Seq.concat_map (fun (hand, st) -> Seq.map (fun st -> hand, st) @@ up_down st)
    |> Seq.map (fun (hand, st) ->
        hand,
        { st with map = List.mapi (fun i s -> if i = st.floor then S.union hand s else s) st.map })
    |> Seq.filter (fun (_, st) -> List.for_all not_fried st.map)
    |> List.of_seq

  let compare a b =
    match Int.compare a.floor b.floor with
    | 0 -> List.compare S.compare a.map b.map
    | v -> v
end

module P = struct
  type space = unit
  type state = State.t
  type data = int * (S.t * int) list
  type weight = int

  let data_id = 0, []

  let is_end _ (s, _) =
    match State.map s with
    | [a; b; c; _] -> List.for_all S.is_empty [a; b; c]
    | _ -> assert false

  let neighbors _ (s, (i, d)) =
    State.neigh s
    |> List.map (fun (hand, s) ->
        s, (i+1, (hand, State.floor s)::d))
end

let solve map =
  let st = State.make map in
  let i, _log = Pathfind.bfs (module State) (module P) () ~start: st in
  (*
  List.rev _log
  |> List.iter (fun (h, i) ->
      S.iter (fun (s, d) -> Printf.printf "[%s %s] " s d) h;
      Printf.printf "to floor %d\n" i
    );
     *)
  i

let extra =
  "an elerium generator. an elerium-compatible microchip. a dilithium generator. a dilithium-compatible microchip."
  |> parse

let () =
  let data = IO.read_lines () |> List.map parse in
  begin
    (* PART 1 *)
    solve data |> Printf.printf "%d\n";

    (* PART 2 *)
    (* TOO SLOW *)
    let data = match data with
      | x :: xs -> S.union x extra :: xs
      | _ -> assert false
    in
    solve data |> Printf.printf "%d\n";
  end
