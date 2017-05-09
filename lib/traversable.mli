(** Traversable signatures *)

module type Basic = sig
  type _ t
  type _ f
  val traverse : ('a -> 'b f) -> 'a t -> 'b t f
  val traverse_ : ('a -> 'b f) -> 'a t -> unit f
end

module type Basic2 = sig
  type _ t
  type (_, _) f
  val traverse : ('a -> ('u, 'b) f) -> 'a t -> ('u, 'b t) f
  val traverse_ : ('a -> ('u, 'b) f) -> 'a t -> ('u, unit) f
end

module type Basic3 = sig
  type _ t
  type (_, _, _) f
  val traverse : ('a -> ('u, 'v, 'b) f) -> 'a t -> ('u, 'v, 'b t) f
  val traverse_ : ('a -> ('u, 'v, 'b) f) -> 'a t -> ('u, 'v, unit) f
end

module type S = sig
  include Basic
  val sequence : 'a f t -> 'a t f
  val foreach : 'a t -> ('a -> 'b f) -> 'b t f
  val sequence_ : 'a f t -> unit f
  val foreach_ : 'a t -> ('a -> 'b f) -> unit f
end

module type S2 = sig
  include Basic2
  val sequence : ('u, 'a) f t -> ('u, 'a t) f
  val foreach : 'a t -> ('a -> ('u, 'b) f) -> ('u, 'b t) f
  val sequence_ : ('u, 'a) f t -> ('u, unit) f
  val foreach_ : 'a t -> ('a -> ('u, 'b) f) -> ('u, unit) f
end

module type S3 = sig
  include Basic3
  val sequence : ('a, 'b, 'c) f t -> ('a, 'b, 'c t) f
  val foreach : 'a t -> ('a -> ('b, 'c, 'd) f) -> ('b, 'c, 'd t) f
  val sequence_ : ('a, 'b, 'c) f t -> ('a, 'b, unit) f
  val foreach_ : 'a t -> ('a -> ('b, 'c, 'd) f) -> ('b, 'c, unit) f
end

module Make (T : Basic) : S with
    type 'a t := 'a T.t and
    type 'a f := 'a T.f

module Make2 (T : Basic2) : S2 with
    type 'a t := 'a T.t and
    type ('u, 'a) f := ('u, 'a) T.f

module Make3 (T : Basic3) : S3 with
    type 'a t := 'a T.t and
    type ('u, 'v, 'a) f := ('u, 'v, 'a) T.f

