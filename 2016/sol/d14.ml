open Ut

let salt = "qzyelonm"
let _salt = "abc" (* debug *)

let md5hex str = Digest.(string str |> to_hex)

let hashes1 =
  Seq.ints 0
  |> Seq.map (fun i -> salt ^ string_of_int i)
  |> Seq.map md5hex
  |> Seq.memoize

let rec repeat i f x =
  if i = 0 then x else repeat (i-1) f (f x)

let hashes2 =
  hashes1
  |> Seq.map (repeat 2016 md5hex)
  |> Seq.memoize

let find_nlet n str =
  String.to_seq str
  |> Seq.group Char.equal
  |> Seq.map (fun s ->
      let c, _ = Seq.uncons s |> Option.get in
      c, Seq.length s)
  |> Seq.find (fun (_, len) -> len >= n)
  |> Option.map fst

let is_key h next =
  find_nlet 3 h
  |> Option.map (fun c ->
      Seq.take 1000 next
      |> Seq.filter_map (find_nlet 5)
      |> Seq.exists ((=) c))
  |> Option.value ~default: false

let keys hashes =
  let rec aux i seq = fun () ->
    match seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (h, next) ->
      if is_key h next
      then Seq.Cons ((i, h), aux (i+1) next)
      else aux (i+1) next ()
  in
  aux 0 hashes

let () =
  let open Seq in
  begin
    (* PART 1 *)
    keys hashes1 |> map fst |> nth_exn 63 |> Printf.printf "%d\n";

    (* PART 2 *)
    keys hashes2 |> map fst |> nth_exn 63 |> Printf.printf "%d\n";
  end
