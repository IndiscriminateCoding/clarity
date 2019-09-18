open Fn

module type Basic = sig
  type 'a t
  val align_as : ('a -> 'b -> 'c) -> ('a -> 'c) -> ('b -> 'c) -> 'a t -> 'b t -> 'c t
end

module type Basic2 = sig
  type ('p, 'a) t
  val align_as :
    ('a -> 'b -> 'c) -> ('a -> 'c) -> ('b -> 'c) -> ('p, 'a) t -> ('p, 'b) t -> ('p, 'c) t
end

module type Basic3 = sig
  type ('p, 'q, 'a) t
  val align_as : ('a -> 'b -> 'c) -> ('a -> 'c) -> ('b -> 'c) ->
    ('p, 'q, 'a) t -> ('p, 'q, 'b) t -> ('p, 'q, 'c) t
end

module type S = sig
  include Basic
  val align_with : (('a, 'b) These.t -> 'c) -> 'a t -> 'b t -> 'c t
  val align : 'a t -> 'b t -> ('a, 'b) These.t t
  val falign : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val pad_zip_with : ('a option -> 'b option -> 'c) -> 'a t -> 'b t -> 'c t
  val pad_zip : 'a t -> 'b t -> ('a option * 'b option) t
end

module type S2 = sig
  include Basic2
  val align_with : (('a, 'b) These.t -> 'c) -> ('p, 'a) t -> ('p, 'b) t -> ('p, 'c) t
  val align : ('p, 'a) t -> ('p, 'b) t -> ('p, ('a, 'b) These.t) t
  val falign : ('a -> 'a -> 'a) -> ('p, 'a) t -> ('p, 'a) t -> ('p, 'a) t
  val pad_zip_with : ('a option -> 'b option -> 'c) -> ('p, 'a) t -> ('p, 'b) t -> ('p, 'c) t
  val pad_zip : ('p, 'a) t -> ('p, 'b) t -> ('p, 'a option * 'b option) t
end

module type S3 = sig
  include Basic3
  val align_with : (('a, 'b) These.t -> 'c) -> ('p, 'q, 'a) t -> ('p, 'q, 'b) t -> ('p, 'q, 'c) t
  val align : ('p, 'q, 'a) t -> ('p, 'q, 'b) t -> ('p, 'q, ('a, 'b) These.t) t
  val falign : ('a -> 'a -> 'a) -> ('p, 'q, 'a) t -> ('p, 'q, 'a) t -> ('p, 'q, 'a) t
  val pad_zip_with :
    ('a option -> 'b option -> 'c) -> ('p, 'q, 'a) t -> ('p, 'q, 'b) t -> ('p, 'q, 'c) t
  val pad_zip : ('p, 'q, 'a) t -> ('p, 'q, 'b) t -> ('p, 'q, 'a option * 'b option) t
end

module Make3 (A : Basic3) = struct
  include A
  let align_with f = align_as
    (fun a b -> f (These.Both (a, b)))
    (f % These._Left)
    (f % These._Right)
  let align a = align_as These._Both These._Left These._Right a
  let falign f = align_as f id id
  let pad_zip_with f = align_as
    (fun a b -> f (Some a) (Some b))
    (fun a -> f (Some a) None)
    (fun b -> f None (Some b))
  let pad_zip x = pad_zip_with (curry id) x
end

module Make2 (A : Basic2) = Make3(struct
  type (_, 'p, 'a) t = ('p, 'a) A.t
  include (A : Basic2 with type ('p, 'a) t := ('p, 'a) A.t)
end)

module Make (A : Basic) = Make2(struct
  type (_, 'a) t = 'a A.t
  include (A : Basic with type 'a t := 'a A.t)
end)

