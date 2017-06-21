(** Monoids *)

module type S = sig
  type t
  val append : t -> t -> t
  val zero : t
end

module Int : sig
  module Min : S with type t = int
  module Max : S with type t = int
  module Sum : S with type t = int
  module Product : S with type t = int
end

module All : S with type t = bool
module Any : S with type t = bool

module Dual (M : S) : S with type t = M.t

module Endo (T : sig type t end) : S with type t = T.t -> T.t

module Pair (M1 : S)(M2 : S) : S with type t = M1.t * M2.t

module Opt (S : Semigroup.S) : S with type t = S.t option

