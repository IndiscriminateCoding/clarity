open Fn

type ('l, 'r) t =
  Left of 'l | Right of 'r

let _Left x = Left x
let _Right x = Right x

let bimap l r = function
  | Left x -> Left (l x)
  | Right x -> Right (r x)

include Monad.Make2(struct
  type nonrec ('l, 'r) t = ('l, 'r) t

  let map f x = bimap id f x

  let pure x = Right x
  let ap = function
    | Left l -> const (Left l)
    | Right f -> fun x -> map f (x ())

  let bind f = function
  | Left l -> Left l
  | Right r -> f r
end)

let fold l r = function
  | Left x -> l x
  | Right x -> r x

let swap = function
  | Left x -> Right x
  | Right x -> Left x

let maybe_left = function
  | Left x -> Some x
  | Right _ -> None

let maybe_right = function
  | Right x -> Some x
  | Left _ -> None

