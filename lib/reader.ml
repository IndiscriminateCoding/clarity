open Fn

type ('r, 'a) t = 'r -> 'a

include Monad.Make2(struct
  type nonrec ('r, 'a) t = ('r, 'a) t

  let map = compose
  let pure = const
  let bind f x =
    fun r -> f (x r) r
  let ap f x r =
    f r (x () r)
end)

external run : ('r, 'a) t -> 'r -> 'a = "%identity"
external reader : ('r -> 'a) -> ('r, 'a) t = "%identity"

let ask : ('r, 'r) t = id
let local : ('r -> 'r) -> ('r, 'a) t -> ('r, 'a) t =
  fun f x -> compose x f

let dimap : ('q -> 'r) -> ('a -> 'b) -> ('r, 'a) t -> ('q, 'b) t =
  fun rf vf x q ->
    vf (x (rf q))

let zip : ('r, 'a) t -> ('r, 'a) t -> ('r, 'a * 'b) t =
  fun a b r -> a r, b r

