(** Void.t is a logically uninhabited data type. *)
type t

(**
  Since Void.t values don't exist, it's logically possible to create value
  of any type
  *)
val absurd : t -> 'a

