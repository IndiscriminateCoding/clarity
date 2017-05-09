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

module Make3(T : Basic3) = struct
  open Fn
  include T

  let sequence x = traverse id x
  let foreach x = flip traverse x
  let sequence_ x = traverse_ id x
  let foreach_ x = flip traverse_ x
end

module Make2(T : Basic2) = struct
  include(Make3(struct
    type 'a t = 'a T.t
    type (_, 'p, 'a) f = ('p, 'a) T.f
    include (T : Basic2 with
      type ('p, 'a) f := ('p, 'a) T.f and
      type 'a t := 'a T.t)
  end))
end

module Make(T : Basic) = struct
  include(Make2(struct
    type 'a t = 'a T.t
    type (_, 'a) f = 'a T.f
    include (T : Basic with
      type 'a f := 'a T.f and
      type 'a t := 'a T.t)
  end))
end

