(** Monad instance for an option type *)
type 'a t = 'a option

val _None : 'a option
val _Some : 'a -> 'a option

include Monad.S with type 'a t := 'a t

val fold : ('a -> 'b) -> (unit -> 'b) -> 'a option -> 'b
val fold' : ('a -> 'b) -> 'b -> 'a option -> 'b
val get_or_else : 'a -> 'a t -> 'a
val to_either : 'l -> 'r t -> ('l, 'r) Either.t
