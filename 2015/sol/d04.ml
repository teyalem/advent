let input = "bgvyzdsv"

let md5hex i =
  input ^ string_of_int i
  |> Digest.string
  |> Digest.to_hex


let part1 () =
  let rec aux i =
    let s = md5hex i in
    if String.sub s 0 5 = "00000"
    then i
    else aux (i+1)
  in
  aux 1

let part2 () =
  let rec aux i =
    let s = md5hex i in
    if String.sub s 0 6 = "000000"
    then i
    else aux (i+1)
  in
  aux 1

let () =
  begin
    Printf.printf "%d\n" @@ part1 ();
    Printf.printf "%d\n" @@ part2 ()
  end
