(** Monoids *)

(** Module-based version *)
module type S = sig
  type t
  val append : t -> t -> t
  val zero : t
end

(** Value version *)
type 'a t = ('a -> 'a -> 'a) * 'a

(** Convert value version to first-class module *)
val pack : 'a t -> (module S with type t = 'a)

(** Unpack module to value *)
val unpack : (module S with type t = 'a) -> 'a t

val to_semigroup : 'a t -> 'a Semigroup.t

val int_min : int t
val int_max : int t
val int_sum : int t
val int_prod : int t
val swap : 'a t -> 'a t
val endo : ('a -> 'a) t
val pair : 'a t -> 'b t -> ('a * 'b) t
val all : bool t
val any : bool t
val list : 'a list t
val option : 'a Semigroup.t -> 'a option t

