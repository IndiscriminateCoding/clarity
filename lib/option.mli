(** Monad instance for an option type *)
type 'a t = 'a option

val _None : 'a option
val _Some : 'a -> 'a option

include Monad.S with type 'a t := 'a t

val fold : ('a -> 'b) -> 'b -> 'a option -> 'b

