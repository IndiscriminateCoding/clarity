(** State monad *)
include Monad.S2

external run : ('s, 'a) t -> 's -> 's * 'a = "%identity"
external state : ('s -> 's * 'a) -> ('s, 'a) t = "%identity"

val get : ('s, 's) t
val gets : ('s -> 'a) -> ('s, 'a) t
val put : 's -> ('s, unit) t
val modify : ('s -> 's) -> ('s, unit) t

