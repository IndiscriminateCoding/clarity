(** Zip datatypes with non-uniform shapes using These.t *)

module type Basic = sig
  type 'a t
  val align_with : (('a, 'b) These.t -> 'c) -> 'a t -> 'b t -> 'c t
end

module type Basic2 = sig
  type ('p, 'a) t
  val align_with :
    (('a, 'b) These.t -> 'c) -> ('p, 'a) t -> ('p, 'b) t -> ('p, 'c) t
end

module type Basic3 = sig
  type ('p, 'q, 'a) t
  val align_with :
    (('a, 'b) These.t -> 'c) ->
    ('p, 'q, 'a) t -> ('p, 'q, 'b) t -> ('p, 'q, 'c) t
end

module type S = sig
  include Basic
  val align : 'a t -> 'b t -> ('a, 'b) These.t t
  val salign : 'a Semigroup.t -> 'a t -> 'a t -> 'a t
  val pad_zip_with : ('a option -> 'b option -> 'c) -> 'a t -> 'b t -> 'c t
  val pad_zip : 'a t -> 'b t -> ('a option * 'b option) t
end

module type S2 = sig
  include Basic2
  val align : ('p, 'a) t -> ('p, 'b) t -> ('p, ('a, 'b) These.t) t
  val salign : 'a Semigroup.t -> ('p, 'a) t -> ('p, 'a) t -> ('p, 'a) t
  val pad_zip_with :
    ('a option -> 'b option -> 'c) ->
    ('p, 'a) t -> ('p, 'b) t -> ('p, 'c) t
  val pad_zip : ('p, 'a) t -> ('p, 'b) t -> ('p, 'a option * 'b option) t
end

module type S3 = sig
  include Basic3
  val align :
    ('p, 'q, 'a) t -> ('p, 'q, 'b) t -> ('p, 'q, ('a, 'b) These.t) t
  val salign :
    'a Semigroup.t -> ('p, 'q, 'a) t -> ('p, 'q, 'a) t -> ('p, 'q, 'a) t
  val pad_zip_with :
    ('a option -> 'b option -> 'c) ->
    ('p, 'q, 'a) t -> ('p, 'q, 'b) t -> ('p, 'q, 'c) t
  val pad_zip :
    ('p, 'q, 'a) t -> ('p, 'q, 'b) t -> ('p, 'q, 'a option * 'b option) t
end

module Make (A : Basic) : S with type 'a t := 'a A.t
module Make2 (A : Basic2) : S2 with type ('p, 'a) t := ('p, 'a) A.t
module Make3 (A : Basic3) : S3 with type ('p, 'q, 'a) t := ('p, 'q, 'a) A.t

