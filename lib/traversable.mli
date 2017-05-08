(** Traversable signatures *)

module type S = sig
  type _ t
  type _ f

  val traverse : ('a -> 'b f) -> 'a t -> 'b t f
  val sequence : 'a f t -> 'a t f
  val foreach : 'a t -> ('a -> 'b f) -> 'b t f
  val traverse_ : ('a -> 'b f) -> 'a t -> unit f
  val sequence_ : 'a f t -> unit f
  val foreach_ : 'a t -> ('a -> 'b f) -> unit f
end

module type S2 = sig
  type _ t
  type (_, _) f

  val traverse : ('a -> ('u, 'b) f) -> 'a t -> ('u, 'b t) f
  val sequence : ('u, 'a) f t -> ('u, 'a t) f
  val foreach : 'a t -> ('a -> ('u, 'b) f) -> ('u, 'b t) f
  val traverse_ : ('a -> ('u, 'b) f) -> 'a t -> ('u, unit) f
  val sequence_ : ('u, 'a) f t -> ('u, unit) f
  val foreach_ : 'a t -> ('a -> ('u, 'b) f) -> ('u, unit) f
end

module type S3 = sig
  type _ t
  type (_, _, _) f

  val traverse : ('a -> ('u, 'v, 'b) f) -> 'a t -> ('u, 'v, 'b t) f
  val sequence : ('a, 'b, 'c) f t -> ('a, 'b, 'c t) f
  val foreach : 'a t -> ('a -> ('b, 'c, 'd) f) -> ('b, 'c, 'd t) f
  val traverse_ : ('a -> ('u, 'v, 'b) f) -> 'a t -> ('u, 'v, unit) f
  val sequence_ : ('a, 'b, 'c) f t -> ('a, 'b, unit) f
  val foreach_ : 'a t -> ('a -> ('b, 'c, 'd) f) -> ('b, 'c, unit) f
end

