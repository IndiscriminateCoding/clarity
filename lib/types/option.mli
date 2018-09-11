(** Monad instance for an option type *)
type 'a t = 'a option

val _None : 'a t
val _Some : 'a -> 'a t

include Monad.S with type 'a t := 'a t

val fold : ('a -> 'b) -> (unit -> 'b) -> 'a t -> 'b
val fold' : ('a -> 'b) -> 'b -> 'a t -> 'b
val get_or_else : 'a -> 'a t -> 'a
val to_right : 'l -> 'r t -> ('l, 'r) Either.t
val to_left : 'r -> 'l t -> ('l, 'r) Either.t
val iter : ('a -> unit) -> 'a t -> unit
