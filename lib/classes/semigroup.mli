(** Semigroups *)

module type S = sig
  type t
  val append : t -> t -> t
end

module First (T : sig type t end) : S with type t = T.t
module Last (T : sig type t end) : S with type t = T.t

