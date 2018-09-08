(** Efficient persistent vectors based on RRB-Trees

 RRB (Relaxed Radix-Balanced) Trees allows "effectively constant"
  operations like getting element / updating at index, appending and
  splitting. *)

type _ t

(** Efficient construction of vectors using mutable, one-by-one appending of elements *)
module Builder : sig
  type 'a vector = 'a t
  type _ t

  (** Creates new empty builder *)
  val empty : unit -> 'a t

  (** Create copy of this builder *)
  val copy : 'a t -> 'a t

  (** Put element to builder *)
  val put : 'a t -> 'a -> unit

  (** Clear builder *)
  val clear : 'a t -> unit

  (** Get vector of current elements in builder *)
  val result : 'a t -> 'a vector
end

exception Out_of_bounds of { index : int; size : int }

(** Empty vector. *)
val empty : 'a t

(** The length of a vector. *)
val length : 'a t -> int

(** [init l f] creates vector of length [l] where value at position [i] is
  initialized with [f i]. *)
val init : int -> (int -> 'a) -> 'a t

(** Concatenates two vectors. "Effectively constant". *)
val append : 'a t -> 'a t -> 'a t

(** [get v i] returns the element of a [v] at position [i]. "Effectively
  constant".

  Raise [Out_of_bounds] when [i] is outside of [v] bounds. *)
val get : 'a t -> int -> 'a

(** [update v i x] returns new vector initialized with values of [v] where
  value at index [i] is replaced with [x]. "Effectively constant".

  Raise [Out_of_bounds] when [i] is outside of [v] bounds. *)
val update : 'a t -> int -> 'a -> 'a t

(** [split_at v i] returns pair of a vectors where first element is the prefix
  of [v] of length [i] and second is the suffix of [v] starting at [i].
  "Effectively constant".

  Raise [Out_of_bounds] when [i] is negative. *)
val split_at : 'a t -> int -> 'a t * 'a t

(** [take v i] returns the prefix of [v] of length [i]. "Effectively
  constant".

  Raise [Out_of_bounds] when [i] is negative. *)
val take : 'a t -> int -> 'a t

(** [drop v i] returns the suffix of [v] starting at [i]. "Effectively
  constant".

  Raise [Out_of_bounds] when [i] is negative. *)
val drop : 'a t -> int -> 'a t

(** [iter f v] iterates over [v] applying [f] to each element. *)
val iter : ('a -> unit) -> 'a t -> unit

(** Converts list to vector *)
val of_list : 'a list -> 'a t

(** Converts vector to list *)
val to_list : 'a t -> 'a list

include Monad.S with type 'a t := 'a t
include Foldable.S with type 'a t := 'a t
include Align.S with type 'a t := 'a t

module A : functor (A : Applicative.Basic) -> Traversable.S with
  type 'a t := 'a t and
  type 'a f := 'a A.t
module A2 : functor (A : Applicative.Basic2) -> Traversable.S2 with
  type 'a t := 'a t and
  type ('u, 'a) f := ('u, 'a) A.t
module A3 : functor (A : Applicative.Basic3) -> Traversable.S3 with
  type 'a t := 'a t and
  type ('u, 'v, 'a) f := ('u, 'v, 'a) A.t

module M : functor (M : Monad.Basic) -> sig
  include Traversable.S with
    type 'a t := 'a t and
    type 'a f := 'a M.t
  include Foldable.M with
    type 'a t := 'a t and
    type 'a m := 'a M.t
end
module M2 : functor (M : Monad.Basic2) -> sig
  include Traversable.S2 with
    type 'a t := 'a t and
    type ('u, 'a) f := ('u, 'a) M.t
  include Foldable.M2 with
    type 'a t := 'a t and
    type ('u, 'a) m := ('u, 'a) M.t
end
module M3 : functor (M : Monad.Basic3) -> sig
  include Traversable.S3 with
    type 'a t := 'a t and
    type ('u, 'v, 'a) f := ('u, 'v, 'a) M.t
  include Foldable.M3 with
    type 'a t := 'a t and
    type ('u, 'v, 'a) m := ('u, 'v, 'a) M.t
end

