open Advent

let base_pattern = [ 0; 1; 0; -1 ]

let repeat_elem n m = List.init n (fun _ -> m)

let make_pattern i n =
  let open List in
  init (n / ((i+1) * length base_pattern) + 1)
    (fun _ -> concat_map (repeat_elem (i+1)) base_pattern)
  |> concat
  |> tl

let rec take i = function
  | [] -> []
  | x::xs ->
    if i = 0
    then []
    else x :: take (i-1) xs

let rec repeat i f (n: 'a) : 'a =
  if i = 1
  then f n
  else repeat (i-1) f (f n)

let phase digits =
  let open List in
  let len = length digits in
  digits
  |> mapi (fun i _ ->
      map2 Int.mul digits
      @@ take len @@ make_pattern i len
      |> sum
      |> (fun n -> abs (n mod 10)))

let reverse_fft tbl offset last : (int, int) Hashtbl.t =
  let ntbl = Hashtbl.(create @@ length tbl) in
  Hashtbl.(add ntbl last (find tbl last));
  for i = last - 1 downto offset do
    let o = Hashtbl.(find tbl i + find ntbl (i+1)) in
    Hashtbl.(replace ntbl i (o mod 10))
  done;
  ntbl

(* Too, too slow.... *)
let main path =
  let data = open_in path |> IO.read_file |> String.to_seq
             |> Seq.map (fun c -> int_of_string @@ String.make 1 c)
             |> List.of_seq
  in
  begin
    (* PART 1 *)
    repeat 100 phase data
    |> take 8
    |> List.iter (fun n -> print_int n);

    print_newline ();

    (* PART 2 *)
    let offset = take 7 data |> List.fold_left (fun p n -> p*10 + n) 0 in
    let len = List.length data in
    let last = len * 10_000 in
    let arr = Array.of_list data in
    let tbl = Hashtbl.create ((last - offset)/len + 1) in
    for i = last downto offset do
      Hashtbl.add tbl i arr.(i mod len)
    done;

    repeat 100 (fun tbl -> reverse_fft tbl offset last) tbl
    |> (fun tbl ->
        for i = 0 to 7 do
          print_int Hashtbl.(find tbl (offset+i))
        done)

  end

let () = Arg.parse [] main ""
