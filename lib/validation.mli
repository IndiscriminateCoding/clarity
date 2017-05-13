(** A data-type like Either but with an error-accumulating Applicative *)

module Make (S : Semigroup.S) : sig
  include Applicative.S

  val fail : S.t -> 'a t
  val map_errors : (S.t -> S.t) -> 'a t -> 'a t

  val fold : (S.t -> 'b) -> ('a -> 'b) -> 'a t -> 'b

  val maybe_errors : 'a t -> S.t option
  val maybe_result : 'a t -> 'a option

  external to_either : 'a t -> (S.t, 'a) Either.t = "%identity"
  external of_either : (S.t, 'a) Either.t -> 'a t = "%identity"

  val zip : 'a t -> 'b t -> ('a * 'b) t
end

