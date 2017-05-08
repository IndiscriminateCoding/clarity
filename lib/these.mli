(** Values of two non-exclusive varians *)
type ('l, 'r) t =
  Left of 'l | Right of 'r | Both of 'l * 'r

val _Left : 'a -> ('a, 'b) t
val _Right : 'a -> ('b, 'a) t
val _Both : 'a -> 'b -> ('a, 'b) t

(** Monad instance requires Semigroup to combine Left values *)
module Make (S : Semigroup.S) : Monad.S with type 'a t = (S.t, 'a) t

val bimap : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
val fold : ('a -> 'b) -> ('c -> 'b) -> ('a -> 'c -> 'b) -> ('a, 'c) t -> 'b
val swap : ('a, 'b) t -> ('b, 'a) t
val maybe_left : ('a, 'b) t -> 'a option
val maybe_right : ('a, 'b) t -> 'b option
val maybe_both : ('a, 'b) t -> ('a * 'b) option

