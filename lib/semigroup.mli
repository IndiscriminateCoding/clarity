(** Semigroups *)

(** Module-based version *)
module type S = sig
  type t
  val append : t -> t -> t
end

(** Value version *)
type 'a t = 'a -> 'a -> 'a

(** Convert value version to first-class module *)
val pack : 'a t -> (module S with type t = 'a)

(** Unpack module to value *)
val unpack : (module S with type t = 'a) -> 'a t

val first : 'a t
val last : 'a t

