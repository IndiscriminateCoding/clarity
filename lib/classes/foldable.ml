module type Basic = sig
  type _ t

  val foldl : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val foldr : ('a -> (unit -> 'b) -> 'b) -> (unit -> 'b) -> 'a t -> 'b
end

module type S = sig
  include Basic

  val foldr' : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
  val fold_map :
    (module Monoid.S with type t = 'm) -> ('a -> 'm) -> 'a t -> 'm
  val any : ('a -> bool) -> 'a t -> bool
  val all : ('a -> bool) -> 'a t -> bool
  val find : ('a -> bool) -> 'a t -> 'a option
end

module Make (F : Basic) = struct
  open Fn
  include F

  let foldr' f a = foldr (fun x a -> f x (a ())) (const a)
  let fold_map (type m) m f =
    let module M = (val m : Monoid.S with type t = m) in
    foldl (fun a x -> M.append a (f x)) M.zero
  let any p = foldr (fun x a -> p x || a ()) (const false)
  let all p = foldr (fun x a -> p x && a ()) (const true)
  let find p = foldr (fun x a -> if p x then Some x else a ()) (const None)
end

module type M = sig
  type _ t
  type _ m

  val foldr_m : ('a -> 'b -> 'b m) -> 'b -> 'a t -> 'b m
  val foldl_m : ('b -> 'a -> 'b m) -> 'b -> 'a t -> 'b m
end

module type M2 = sig
  type _ t
  type (_, _) m

  val foldr_m : ('a -> 'b -> ('u, 'b) m) -> 'b -> 'a t -> ('u, 'b) m
  val foldl_m : ('b -> 'a -> ('u, 'b) m) -> 'b -> 'a t -> ('u, 'b) m
end

module type M3 = sig
  type _ t
  type (_, _, _) m

  val foldr_m : ('a -> 'b -> ('u, 'v, 'b) m) -> 'b -> 'a t -> ('u, 'v, 'b) m
  val foldl_m : ('b -> 'a -> ('u, 'v, 'b) m) -> 'b -> 'a t -> ('u, 'v, 'b) m
end

