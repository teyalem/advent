type t

type program = int list

val load : program -> t

val copy : t -> t

val is_halt : t -> bool

val code_length : t -> int

val peek : t -> int -> int
val poke : t -> int -> int -> unit

val set_interactive : t -> bool -> unit

val is_output_empty : t -> bool

val push_input : t -> int -> unit
val pop_output : t -> int

val flush_ascii : t -> int
val collect_output : t -> int list
val print_output : t -> unit

val run : t -> unit

val parse_code : string -> program
