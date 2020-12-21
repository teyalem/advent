open Advent

(* Codes of docking program *)
module Code = struct
  type t = Mask of (int * int) list
         | Assign of int * int

  let parse_mask str =
    String.to_seq str
    |> List.of_seq
    |> List.mapi (fun i c ->
        match c with
        | 'X' -> i, -1
        | '0' -> i, 0
        | '1' -> i, 1
        | _ -> assert false)
    |> List.map (fun (i, n) -> 35 - i, n) (* just used fixed number *)

  let parse str =
    if starts_with "mask" str then
      Scanf.sscanf str "mask = %s" (fun s -> Mask (parse_mask s))
    else
      Scanf.sscanf str "mem[%d] = %d" (fun i n -> Assign (i, n))

end

(* Memory *)
module Memory = struct
  type t = (int * int) list (* association list *)
  (* in fact, when using array instead of this, you'll need over 8GB ram. *)

  let empty = []

  let set mem i n =
    let mem = if List.mem_assoc i mem
      then List.remove_assoc i mem
      else mem
    in 
      (i, n) :: mem

  let get_data mem = List.split mem |> snd
end

(* apply mask of PART 1 *)
let apply_mask mask n =
  let rec inner bits = function
    | [] -> bits
    | (i, n) :: mask ->
      begin
        if n = -1 then ()
        else Bitarray.set bits i n;
      end;
      inner bits mask
  in
  let bits = Bitarray.of_int n in
  inner bits mask |> Bitarray.to_int

(* code runner of PART 1 *)
let run_1 mem code =
  let rec inner mem mask = function
    | [] -> mem
    | Code.Mask new_mask :: code -> inner mem new_mask code
    | Code.Assign (i, n) :: code ->
      let n = apply_mask mask n in
      let mem = Memory.set mem i n in
      inner mem mask code
  in
  inner mem [] code

(* apply mask of PART 2 *)
let apply_mask_2 mask idx =
  let rec inner bsl = function
    | [] -> bsl
    | (i, n) :: mask -> begin
        match n with
        | -1 ->
          let os = List.map Array.copy bsl in
          List.iter (fun bs -> Bitarray.set bs i 0) os;
          List.iter (fun bs -> Bitarray.set bs i 1) bsl;
          inner (os @ bsl) mask
        | 0 -> inner bsl mask
        | 1 ->
          List.iter (fun bs -> Bitarray.set bs i 1) bsl;
          inner bsl mask
        | _ -> assert false
      end
  in
  let bs = Bitarray.of_int idx in
  inner [bs] mask
  |> List.map Bitarray.to_int

(* code runner of PART 2 *)
let run_2 mem code =
  let rec inner mem mask = function
    | [] -> mem
    | Code.Mask new_mask :: code -> inner mem new_mask code
    | Code.Assign (i, n) :: code ->
      let indexes = apply_mask_2 mask i in
      let mem = List.fold_left
          (fun mem i -> Memory.set mem i n)
          mem indexes
      in
      inner mem mask code
  in
  inner mem [] code

let main path =
  let data = open_in path
             |> IO.read_lines
             |> List.map Code.parse
  in
  let mem = Memory.empty in
  begin
    (* PART 1 *)
    run_1 mem data
    |> Memory.get_data
    |> sum |> print_int;

    print_newline ();

    (* PART 2 *)
    run_2 mem data
    |> Memory.get_data
    |> sum |> print_int
  end

let _ = Arg.parse [] main ""
