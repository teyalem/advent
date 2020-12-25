open Advent

module Handshake = struct
  let subject_number = 7
  let dividing_number = 20201227

  let step sub key =
    (key * sub) mod dividing_number

  let transform loop_size key =
    let rec loop i tmp =
      if i = loop_size then tmp
      else
        loop (i + 1) @@ step key tmp
    in
    loop 0 1

  let find_loop_size pubkey =
    let rec loop i key =
      if key = pubkey
      then i
      else
        loop (i+1) @@ step subject_number key
    in
    loop 0 1

end

let parse_pubkeys keys =
  let keys = List.map int_of_string keys in
  match keys with
  | [ card; door ] -> card, door
  | _ -> raise (Invalid_argument "parse_pubkeys")

let main path =
  let card, door = open_in path |> IO.read_lines |> parse_pubkeys in
  begin
    (* PART 1 *)
    let card_lsize = Handshake.find_loop_size card in
    let pub_key = Handshake.transform card_lsize door in
    print_int pub_key

  end

let _ = Arg.parse [] main ""
