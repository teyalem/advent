(* Block of single elements *)

(* Sign Type *)
module type SignType = sig
  type t

  val default: t

  val of_char: char -> t
  val to_char: t -> char
end

(* Output module *)
module type S = sig
  type elt
  type t

  val make: int -> int -> t

  val get: t -> int -> int -> elt
  val set: t -> int -> int -> elt -> unit

  val dimx: t -> int
  val dimy: t -> int

  (* iterations *)
  val iteri: (int -> int -> elt -> unit) -> t -> unit

  val print: t -> unit
  val parse: string -> t

  val count: elt -> t -> int
end

module Make(Sign: SignType) = struct
  type elt = Sign.t
  type t = elt array array

  (* make empty block *)
  let make dimx dimy = Array.make_matrix dimx dimy Sign.default

  (* get/set a position of block *)
  let get block x y = block.(x).(y)
  let set block x y n = block.(x).(y) <- n

  let dimx block = Array.length block
  let dimy block = Array.length block.(0)

  (* iterate through block *)
  let iteri f block =
    for x = 0 to Array.length block - 1 do
      for y = 0 to Array.length block.(0) - 1 do
        f x y (get block x y)
      done
    done

  (* debug: print block *)
  let print block =
    for y = 0 to dimy block - 1 do
      for x = 0 to dimx block - 1 do
        get block x y |> Sign.to_char |> print_char
      done;
      print_newline ()
    done;
    print_newline ()

  (* parse a matrix *)
  let parse sl =
    (* converting string list to char list list *)
    let sl = List.map (fun s -> String.to_seq s |> List.of_seq) sl in
    let dimx = List.(length (hd sl))
    and dimy = List.length sl in
    let mat = make dimx dimy in
    sl |> List.iteri (fun y s ->
         s |> List.iteri (fun x c -> set mat x y (Sign.of_char c)));
    mat

  (* count occurence of element t *)
  let count_occur t block =
    let count = ref 0 in
    iteri (fun _ _ c ->
        if c = t then incr count else ()) block;
    !count

end
