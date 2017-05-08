(** Identity type *)
type 'a t = 'a

include Monad.Basic with type 'a t := 'a t

