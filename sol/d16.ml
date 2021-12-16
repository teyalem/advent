open Ut

let con k ns : int =
  List.fold_left (fun n b -> k*n + b) 0 ns

let bits_to_num b : int =
  List.of_seq b |> con 2

module BITS = struct
  type t = {
    version : int;
    typeid : int;
    data : data;
  }

  and data =
    | Literal of int
    | Packets of t list

  let literal version n =
    { version; typeid = 4; data = Literal n }

  let pack version typeid packs =
    { version; typeid; data = Packets packs }

  let rec parse bs : t * int Seq.t =
    let nbits n bs : int * int Seq.t =
      (bits_to_num @@ Seq.take n bs), Seq.drop n bs
    in

    let rec lit bs =
      let s, bs = nbits 1 bs in
      let n, bs = nbits 4 bs in
      if s = 0 then (* last *)
        [n], bs
      else
        let ns, bs = lit bs in
        n::ns, bs
    in

    let ver, bs = nbits 3 bs in
    let id, bs = nbits 3 bs in
    if id = 4 then (* literal *)
      let ns, bs = lit bs in
      literal ver (con 16 ns), bs

    else
      let l, bs = nbits 1 bs in

      if l = 0 then
        let tl, bs = nbits 15 bs in
        let sub = Seq.take tl bs in
        let bs = Seq.drop tl bs in
        let rec aux bs =
          if bs () = Seq.Nil then []
          else
            let p, bs = parse bs in
            let ps = aux bs in
            p::ps
        in
        let ps = aux sub in
        pack ver id ps, bs

      else
        let np, bs = nbits 11 bs in
        let rec aux i bs : t list * int Seq.t =
          if i = 0 then [], bs
          else
            let p, bs = parse bs in
            let ps, bs = aux (i-1) bs in
            p::ps, bs
        in
        let ps, bs = aux np bs in
        pack ver id ps, bs

  let rec sum_version { version; data } =
    version +
    match data with
    | Literal _ -> 0
    | Packets ps -> List.map sum_version ps |> sum

  let rec evaluate { typeid; data } =
    let data = match data with
      | Literal n -> [n]
      | Packets ps -> List.map evaluate ps
    in

    let open List in
    match typeid with
    | 0 -> fold_left Int.add 0 data
    | 1 -> fold_left Int.mul 1 data
    | 2 -> fold_left min Int.max_int data
    | 3 -> fold_left max Int.min_int data
    | 4 -> hd data
    | 5 -> if nth data 0 > nth data 1 then 1 else 0
    | 6 -> if nth data 0 < nth data 1 then 1 else 0
    | 7 -> if nth data 0 = nth data 1 then 1 else 0
    | _ -> assert false
end

let hex_to_bits str =
  let hex c =
    if '0' <= c && c <= '9' then
      Char.(code c - code '0')
    else if 'A' <= c && c <= 'F' then
      Char.(code c - code 'A' + 10)
    else
      assert false
  in
  let dec n =
    let f i = (n lsr i) land 1 in
    List.map f [ 3; 2; 1; 0 ] |> List.to_seq
  in
  String.to_seq str |> Seq.map hex |> Seq.concat_map dec

let () =
  let data, _ = IO.read_all () |> hex_to_bits |> BITS.parse in
  begin
    (* PART 1 *)
    BITS.sum_version data |> Printf.printf "%d\n";

    (* PART 2 *)
    BITS.evaluate data |> Printf.printf "%d\n";
  end
