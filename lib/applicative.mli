(** Applicative functors *)

module type Basic = sig
  include Functor.Basic
  val pure : 'a -> 'a t
  val ap : ('a -> 'b) t -> (unit -> 'a t) -> 'b t
end

module type Basic2 = sig
  include Functor.Basic2
  val pure : 'a -> ('p, 'a) t
  val ap : ('p, 'a -> 'b) t -> (unit -> ('p, 'a) t) -> ('p, 'b) t
end

module type Basic3 = sig
  include Functor.Basic3
  val pure : 'a -> ('p, 'q, 'a) t
  val ap : ('p, 'q, 'a -> 'b) t -> (unit -> ('p, 'q, 'a) t) -> ('p, 'q, 'b) t
end

module type S = sig
  type _ t
  include Basic with type 'a t := 'a t
  include Functor.S with type 'a t := 'a t
  val (<*>) : ('a -> 'b) t -> (unit -> 'a t) -> 'b t
  val discard_left : 'a t -> (unit -> 'b t) -> 'b t
  val discard_right : 'a t -> (unit -> 'b t) -> 'a t
  val repeat : int -> 'a t -> 'a list t
  val repeat_ : int -> 'a t -> unit t
  val forever : 'a t -> 'b t
end

module type S2 = sig
  type (_, _) t
  include Basic2 with type ('p, 'a) t := ('p, 'a) t
  include Functor.S2 with type ('p, 'a) t := ('p, 'a) t
  val (<*>) : ('p, 'a -> 'b) t -> (unit -> ('p, 'a) t) -> ('p, 'b) t
  val discard_left : ('p, 'a) t -> (unit -> ('p, 'b) t) -> ('p, 'b) t
  val discard_right : ('p, 'a) t -> (unit -> ('p, 'b) t) -> ('p, 'a) t
  val repeat : int -> ('p, 'a) t -> ('p, 'a list) t
  val repeat_ : int -> ('p, 'a) t -> ('p, unit) t
  val forever : ('p, 'a) t -> ('p, 'b) t
end

module type S3 = sig
  type (_, _, _) t
  include Basic3 with type ('p, 'q, 'a) t := ('p, 'q, 'a) t
  include Functor.S3 with type ('p, 'q, 'a) t := ('p, 'q, 'a) t
  val (<*>) :
    ('p, 'q, 'a -> 'b) t -> (unit -> ('p, 'q, 'a) t) -> ('p, 'q, 'b) t
  val discard_left :
    ('p, 'q, 'a) t -> (unit -> ('p, 'q, 'b) t) -> ('p, 'q, 'b) t
  val discard_right :
    ('p, 'q, 'a) t -> (unit -> ('p, 'q, 'b) t) -> ('p, 'q, 'a) t
  val repeat : int -> ('p, 'q, 'a) t -> ('p, 'q, 'a list) t
  val repeat_ : int -> ('p, 'q, 'a) t -> ('p, 'q, unit) t
  val forever : ('p, 'q, 'a) t -> ('p, 'q, 'b) t
end

module Make (A : Basic) : S with type 'a t := 'a A.t
module Make2 (A : Basic2) : S2 with type ('p, 'a) t := ('p, 'a) A.t
module Make3 (A : Basic3) : S3 with type ('p, 'q, 'a) t := ('p, 'q, 'a) A.t

