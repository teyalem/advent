(* Functor for Cellular Automata *)

(* Position module for cellular automata *)
module type PositionType = sig
  type t
  val neighbor_positions: t -> t list
end

(* Cell *)
module type CellType = sig
  type t
  val default: t
  val next: t list -> t -> t
end

(* Board for Cellular Automata *)
module type BoardType = sig
  type elt
  type pos
  type t

  val get_cell: t -> pos -> elt option
  val set_cell: t -> pos -> elt -> unit

  val iteri_cell: (pos -> elt -> unit) -> t -> unit
end

(* Cellular Automata Type *)
module type S = sig
  type elt
  type pos
  type t

  (* convenient functions for extending module *)
  val default: elt
  val next: elt list -> elt -> elt
  val neighbor_positions: pos -> pos list

  (* main function *)
  val next_state: t -> t
end

(* Cellular Automata Functor *)
module Make (B: BoardType)
    (P: PositionType with type t = B.pos)
    (C: CellType with type t = B.elt)
  : (S with type t := B.t
        and type elt := B.elt
        and type pos := B.pos) =
struct

  let default = C.default
  let next = C.next
  let neighbor_positions = P.neighbor_positions

  let neighbors board pos =
    let neighs = neighbor_positions pos in
    List.map
      (fun pos -> B.get_cell board pos
                  |> Option.value ~default: default)
      neighs

  let next_state board =
    let updates = ref [] in (* update list *)
    let collect_updates pos elt =
        let neighs = neighbors board pos in
        let next_elt = next neighs elt in

        (* update only if the element is changed *)
        if next_elt <> elt
        then updates := (pos, next_elt) :: !updates
        else ()
    in

    B.iteri_cell collect_updates board;
    List.iter (fun (pos, elt) -> B.set_cell board pos elt) !updates;
    board

end
