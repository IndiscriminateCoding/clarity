(** Functors *)

module type Basic = sig
  type _ t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type Basic2 = sig
  type (_, _) t
  val map : ('a -> 'b) -> ('p, 'a) t -> ('p, 'b) t
end

module type Basic3 = sig
  type (_, _, _) t
  val map : ('a -> 'b) -> ('p, 'q, 'a) t -> ('p, 'q, 'b) t
end

module type S = sig
  include Basic
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val replace : 'a -> 'b t -> 'a t
  val void : 'a t -> unit t
end

module type S2 = sig
  include Basic2
  val (>|=) : ('p, 'a) t -> ('a -> 'b) -> ('p, 'b) t
  val replace : 'a -> ('p, 'b) t -> ('p, 'a) t
  val void : ('p, 'a) t -> ('p, unit) t
end

module type S3 = sig
  include Basic3
  val (>|=) : ('p, 'q, 'a) t -> ('a -> 'b) -> ('p, 'q, 'b) t
  val replace : 'a -> ('p, 'q, 'b) t -> ('p, 'q, 'a) t
  val void : ('p, 'q, 'a) t -> ('p, 'q, unit) t
end

module Make (F : Basic) : S with type 'a t := 'a F.t
module Make2 (F : Basic2) : S2 with type ('p, 'a) t := ('p, 'a) F.t
module Make3 (F : Basic3) : S3 with type ('p, 'q, 'a) t := ('p, 'q, 'a) F.t

