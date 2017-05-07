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
  val ap :
    ('p, 'q, 'a -> 'b) t ->
    (unit -> ('p, 'q, 'a) t) -> ('p, 'q, 'b) t
end

module type S = sig
  type _ t
  include Basic with type 'a t := 'a t
  include Functor.S  with type 'a t := 'a t
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
  include Functor.S2  with type ('p, 'a) t := ('p, 'a) t
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
  include Functor.S3  with type ('p, 'q, 'a) t := ('p, 'q, 'a) t
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

module Make3(A : Basic3) = struct
  open Fn
  include Functor.Make3(A)
  include A

  let (<*>) = ap
  let discard_left l r = map (flip const) l <*> r
  let discard_right l r = map const l <*> r
  let repeat cnt x =
    let rec r cnt (xl : ('a, 'b, 'c list) t) =
      if cnt <= 0
      then xl
      else r (cnt - 1) (map (fun h t -> h :: t) x <*> const xl)
    in
    r cnt (pure [])
  let repeat_ cnt x =
    let rec r cnt xl =
      if cnt <= 0
      then xl
      else r (cnt - 1) (discard_left x (const xl))
    in
    r cnt (pure ())
  let rec forever x = discard_left x (forever @! x)
end

module Make2(A : Basic2) = struct
  include(Make3(struct
    type (_, 'p, 'a) t = ('p, 'a) A.t
    include (A : Basic2 with type ('p, 'a) t := ('p, 'a) A.t)
  end))
end

module Make(A : Basic) = struct
  include(Make2(struct
    type (_, 'a) t = 'a A.t
    include (A : Basic with type 'a t := 'a A.t)
  end))
end

