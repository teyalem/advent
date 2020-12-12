(* calculate difference of jolts between two neighboring adapters. *)
let rec diff = function
  | [] | [_] -> []
  | a::b::ns ->
    (b - a) :: (diff (b::ns))

(* count possible arranges of adapters by tribonaci algorithm. O(n). *)
let count_arranges data =
  let max_i = List.fold_left max 0 data in
  let arr = Array.make (max_i+1) 0 in
  arr.(0) <- 1;
  let get i = match arr.(i) with (* Array.get with default *)
    | n -> n
    | exception Invalid_argument _ -> 0
  in
  data
  |> List.iter (fun i ->
      arr.(i) <- (get (i-1)) + (get (i-2)) + (get (i-3)));
  get max_i


let main path =
  (* sorted list of adapters *)
  let data = open_in path
             |> Util.read_lines
             |> List.map int_of_string
             |> List.sort Int.compare
  in
  begin
    (* PART 1 *)

    (* differences of jolts between two neighboring adapters *)
    let diff = diff (0::data) |> fun d -> d@[3] (* last-to-device *)
    in
    let ones = List.(filter ((=) 1) diff |> length)
    and threes = List.(filter ((=) 3) diff |> length)
    in print_int (ones * threes);

    print_newline ();

    (* PART 2 *)
    let arranges = count_arranges data in
    print_int arranges
  end

let _ = Arg.parse [] main ""
