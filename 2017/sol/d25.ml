open Ut

module S = Set.Make(Int)

type turing = {
  state: string;
  cur: int;
  ones: S.t; (* position of ones on tape *)
}

and states = (string * state_inst) list
and state_inst = inst * inst (* 0, 1 *)
and inst = {
  write_one: bool;
  delta: int;
  next_state: string;
}

let make_turing state =
  { state; cur = 0; ones = S.empty }

let step states { state; cur; ones } =
  let z, o = List.assoc state states in
  let { write_one; delta; next_state } =
    if not @@ S.mem cur ones then z else o
  in
  { state = next_state;
    cur = cur + delta;
    ones = (if write_one then S.add else S.remove) cur ones; }

let parse_state str =
  let aux str =
    Scanf.sscanf str
      " If the current value is %d:
      - Write the value %d.
      - Move one slot to the %s@.
      - Continue with state %s@."
      (fun _ w d s -> {
           write_one = w = 1;
           delta = if d = "left" then -1 else 1;
           next_state = s; })
  in
  match String.split_on_char '\n' str with
  | s :: rest ->
    let z, o =
      List.filteri (fun i _ -> i < 4) rest |> String.concat "\n",
      List.filteri (fun i _ -> i >= 4) rest |> String.concat "\n"
    in
    Scanf.sscanf s "In state %s@:" Fun.id, (aux z, aux o)
  | _ -> assert false

let parse str =
  let aux str =
    Scanf.sscanf str "Begin in state %s@.
      Perform a diagnostic checksum after %d steps."
      (fun s d -> s, d)
  in
  match Str.(split (regexp "\n\n") str) with
  | head :: states ->
    let s, d = aux head in
    s, d, List.map parse_state states
  | _ -> assert false

let cksum { ones; _ } =
  S.cardinal ones

let solve s d states =
  let turing = make_turing s in
  Seq.iterate (step states) turing
  |> Seq.drop d
  |> Seq.take 1
  |> Seq.map cksum
  |> Seq.iter print_int

let () =
  let s, d, data = IO.read_all () |> parse in
  solve s d data
