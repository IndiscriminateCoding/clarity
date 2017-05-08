module type S = sig
  type _ t

  val foldl : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val foldr : ('a -> (unit -> 'b) -> 'b) -> 'b -> 'a t -> 'b
  val fold_map : 'a Monoid.t -> ('a -> 'a) -> 'a t -> 'a
  val any : ('a -> bool) -> bool -> 'a t -> bool
  val all : ('a -> bool) -> bool -> 'a t -> bool
end

module type M = sig
  type _ t
  type _ m

  include Traversable.S with
    type 'a t := 'a t and
    type 'a f := 'a m

  val foldr_m :
    ('a -> 'b -> 'b m) -> 'b -> 'a t -> 'b m
  val foldl_m :
    ('b -> 'a -> 'b m) -> 'b -> 'a t -> 'b m
end

module type M2 = sig
  type _ t
  type (_, _) m

  include Traversable.S2 with
    type 'a t := 'a t and
    type ('u, 'a) f := ('u, 'a) m

  val foldr_m :
    ('a -> 'b -> ('u, 'b) m) -> 'b -> 'a t -> ('u, 'b) m
  val foldl_m :
    ('b -> 'a -> ('u, 'b) m) -> 'b -> 'a t -> ('u, 'b) m
end

module type M3 = sig
  type _ t
  type (_, _, _) m

  include Traversable.S3 with
    type 'a t := 'a t and
    type ('u, 'v, 'a) f := ('u, 'v, 'a) m

  val foldr_m :
    ('a -> 'b -> ('u, 'v, 'b) m) -> 'b -> 'a t -> ('u, 'v, 'b) m
  val foldl_m :
    ('b -> 'a -> ('u, 'v, 'b) m) -> 'b -> 'a t -> ('u, 'v, 'b) m
end

