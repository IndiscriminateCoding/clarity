(** Efficient persistent vectors *)

module Internal : sig
  type 'a t =
    | Leaf of 'a array
    | R_node of int array * 'a t array
    | B_node of 'a t array

  val _BRANCHING : int
  val _BITS : int

  val depth : 'a t -> int
  val mk_rnode : 'a t array -> 'a t
  val rr_search : int array -> int -> int -> int * int
end

type 'a t = 'a Internal.t

exception Out_of_bounds of { index : int; size : int }

val empty : 'a t
val length : 'a t -> int
val init : int -> (int -> 'a) -> 'a t
val append : 'a t -> 'a t -> 'a t
val get : 'a t -> int -> 'a
val update : 'a t -> int -> 'a -> 'a t
val split_at : 'a t -> int -> 'a t * 'a t
val take : 'a t -> int -> 'a t
val drop : 'a t -> int -> 'a t
val iter : ('a -> unit) -> 'a t -> unit

include Monad.S with type 'a t := 'a t
include Foldable.S with type 'a t := 'a t
include Align.S with type 'a t := 'a t

