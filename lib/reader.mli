(** Reader monad *)
include Monad.S2

external run : ('r, 'a) t -> 'r -> 'a = "%identity"
external reader : ('r -> 'a) -> ('r, 'a) t = "%identity"

val ask : ('r, 'r) t
val local : ('r -> 'r) -> ('r, 'a) t -> ('r, 'a) t
val dimap : ('q -> 'r) -> ('a -> 'b) -> ('r, 'a) t -> ('q, 'b) t
val zip : ('r, 'a) t -> ('r, 'b) t -> ('r, 'a * 'b) t

