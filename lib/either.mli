type ('l, 'r) t =
  Left of 'l | Right of 'r

val _Left : 'a -> ('a, 'b) t
val _Right : 'a -> ('b, 'a) t

include Monad.S2 with type ('l, 'r) t := ('l, 'r) t

val bimap : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
val fold : ('a -> 'b) -> ('c -> 'b) -> ('a, 'c) t -> 'b
val swap : ('a, 'b) t -> ('b, 'a) t
val maybe_left : ('a, 'b) t -> 'a option
val maybe_right : ('a, 'b) t -> 'b option

