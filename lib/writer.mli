(** Writer monad *)
module Make : functor (M : Monoid.S) -> sig
  include Monad.S

  external run : 'a t -> M.t * 'a = "%identity"
  val writer : 'a -> M.t -> 'a t

  val tell : M.t -> unit t
  val listen : 'a t -> ('a * M.t) t
  val censor : (M.t -> M.t) -> 'a t -> 'a t
  val zip : 'a t -> 'b t -> ('a * 'b) t
end

