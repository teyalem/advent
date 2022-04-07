open Ut

module Pixel = struct
  type t = Black
         | White
         | Transparent

  let default = Transparent

  let of_char c =
    match c with
    | '0' -> Black
    | '1' -> White
    | '2' -> Transparent
    | _ -> assert false

  (* to print *)
  let to_char c =
    match c with
    | Black -> '.'
    | White -> '#'
    | Transparent -> '?'

end

module Image = struct

  include Block.Make(Pixel)

  let width = 25
  let height = 6

  let rec parse_layers str =
    let img, str = parse_raw width height str in
    if str = ""
    then [img]
    else img :: parse_layers str

  let render layers =
    let rec find_first_nontp x y = function
      | [] -> failwith "all pixels are transparent"
      | img :: rest ->
        let c = get img x y in
        if c = Pixel.Transparent
        then find_first_nontp x y rest
        else c
    in
    let img = make width height in
    iteri (fun x y _ -> set img x y @@ find_first_nontp x y layers) img;
    img
    
end

let select f mf a b =
  if f (mf a) (mf b)
  then a
  else b

let main path =
  let data = open_in path |> IO.read_file |> Image.parse_layers in
  begin
    (* PART 1 *)
    let fewest_zeros = List.fold_left
        (fun p n -> select (<) (Image.count_occur (Pixel.of_char '0')) p n)
        (List.hd data)
        (List.tl data)
    in
    Image.(count_occur (Pixel.of_char '1') fewest_zeros
           * count_occur (Pixel.of_char '2') fewest_zeros)
    |> print_int;

    print_newline ();

    (* PART 2 *)
    let img = Image.render data in
    Image.print img

  end

let () = Arg.parse [] main ""
