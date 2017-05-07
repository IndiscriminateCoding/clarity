module type Basic = sig
  include Applicative.Basic
  val bind : ('a -> 'b t) -> 'a t -> 'b t
end

module type Basic2 = sig
  include Applicative.Basic2
  val bind : ('a -> ('p, 'b) t) -> ('p, 'a) t -> ('p, 'b) t
end

module type Basic3 = sig
  include Applicative.Basic3
  val bind : ('a -> ('p, 'q, 'b) t) -> ('p, 'q, 'a) t -> ('p, 'q, 'b) t
end

module type S = sig
  type _ t
  include Basic with type 'a t := 'a t
  include Applicative.S with type 'a t := 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val join : 'a t t -> 'a t
  val mcompose : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t
end

module type S2 = sig
  type (_, _) t
  include Basic2 with type ('p, 'a) t := ('p, 'a) t
  include Applicative.S2 with type ('p, 'a) t := ('p, 'a) t
  val ( >>= ) : ('p, 'a) t -> ('a -> ('p, 'b) t) -> ('p, 'b) t
  val join : ('p, ('p, 'a) t) t -> ('p, 'a) t
  val mcompose :
    ('b -> ('p, 'c) t) -> ('a -> ('p, 'b) t) -> 'a -> ('p, 'c) t
end

module type S3 = sig
  type (_, _, _) t
  include Basic3 with type ('p, 'q, 'a) t := ('p, 'q, 'a) t
  include Applicative.S3 with type ('p, 'q, 'a) t := ('p, 'q, 'a) t
  val ( >>= ) : ('p, 'q, 'a) t -> ('a -> ('p, 'q, 'b) t) -> ('p, 'q, 'b) t
  val join : ('p, 'q, ('p, 'q, 'a) t) t -> ('p, 'q, 'a) t
  val mcompose :
    ('b -> ('p, 'q, 'c) t) -> ('a -> ('p, 'q, 'b) t) -> 'a -> ('p, 'q, 'c) t
end

module Make (M : Basic) : S with type 'a t := 'a M.t
module Make2 (M : Basic2) : S2 with type ('p, 'a) t := ('p, 'a) M.t
module Make3 (M : Basic3) : S3 with type ('p, 'q, 'a) t := ('p, 'q, 'a) M.t

