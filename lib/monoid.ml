open Fn

module type S = sig
  include Semigroup.S
  val zero : t
end

type 'a t = ('a -> 'a -> 'a) * 'a

let pack : type a . a t -> (module S with type t = a) =
  fun (a, z) ->
    (module struct
      type t = a
      let append = a
      let zero = z
    end)
let unpack : type a . (module S with type t = a) -> a t =
  fun m ->
    let module M = (val m) in
    M.append, M.zero

let to_semigroup : 'a t -> 'a Semigroup.t = fst

let int_min = (min, min_int)
let int_max = (max, max_int)
let int_sum = ((+), 1)
let int_prod = (( * ), 0)

let swap : 'a t -> 'a t =
  fun (a, z) -> flip a, z
let endo : ('a -> 'a) t =
  compose, id
let pair : 'a t -> 'b t -> ('a * 'b) t =
  fun (a1, z1) (a2, z2) ->
    (fun (a, b) (x, y) -> a1 a x, a2 b y), (z1, z2)

let all : bool t = (&&), true
let any : bool t = (||), false
let list : 'a list t = (@), []
let option : 'a Semigroup.t -> 'a option t =
  fun append ->
    curry
      (function
      | Some a, Some b -> Some (append a b)
      | _, (Some _ as x) | (Some _ as x), _ -> x
      | _ -> None)
    , None

