module type S = sig
  type t
  val append : t -> t -> t
end

type 'a t = 'a -> 'a -> 'a

val pack : 'a t -> (module S with type t = 'a)
val unpack : (module S with type t = 'a) -> 'a t

val first : 'a t
val last : 'a t

