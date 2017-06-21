open Fn

type ('s, 'a) t = 's -> ('s * 'a)

include Monad.Make2 (struct
  type nonrec ('s, 'a) t = ('s, 'a) t

  let map f x s =
    let s, x = x s in
    s, f x
  let pure x = fun s -> s, x
  let bind f x s =
    let s', x' = x s in
    f x' s'
  let ap f x s =
    let sa, f' = f s in
    let sb, x' = x () sa in
    sb, f' x'
end)

external run : ('s, 'a) t -> 's -> ('s * 'a) = "%identity"
external state : ('s -> ('s * 'a)) -> ('s, 'a) t = "%identity"

let get : ('s, 's) t = fun s -> s, s
let gets : ('s -> 'a) -> ('s, 'a) t =
  fun f s -> s, f s
let put : 's -> ('s, unit) t =
  fun s _ -> s, ()
let modify : ('s -> 's) -> ('s, unit) t =
  fun f s -> f s, ()

