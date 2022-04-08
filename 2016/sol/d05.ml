open Ut

let gen_pass to_string id =
  Seq.ints 0
  |> Seq.map (fun i -> Digest.(to_hex @@ string @@ id ^ string_of_int i))
  |> Seq.filter (String.starts_with ~prefix: "00000")
  |> to_string

let to_string1 seq =
  seq
  |> Seq.map (fun md5 -> md5.[5])
  |> Seq.take 8
  |> String.of_seq

let to_string2 seq =
  let buf = Bytes.make 8 '_' in
  let f (i, c) =
    if 0 <= i && i <= 7 && Bytes.get buf i = '_' then
      Bytes.set buf i c;
    (* exit condition check *)
    if Bytes.for_all ((<>) '_') buf then raise Exit
  in
  let pull md5 = Char.(code md5.[5] - code '0'), md5.[6] in
  try
    seq |> Seq.map pull |> Seq.iter f;
    assert false
  with Exit -> Bytes.to_string buf

let door_id = "ugkcyxxp"

(* takes some time to run *)
let () =
  begin
    (* PART 1 *)
    gen_pass to_string1 door_id |> Printf.printf "%s\n";

    (* PART 2 *)
    gen_pass to_string2 door_id |> Printf.printf "%s\n";
  end
