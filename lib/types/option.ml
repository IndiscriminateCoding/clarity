open Fn

type 'a t = 'a option

let _None = None
let _Some x = Some x

include Monad.Make(struct
  type nonrec 'a t = 'a t

  let map f = function
  | None -> None
  | Some x -> Some (f x)

  let pure x = Some x
  let ap = function
    | None -> const None
    | Some f -> fun x -> map f (x ())

  let bind f = function
  | None -> None
  | Some x -> f x
end)

let fold s n = function
  | None -> n ()
  | Some x -> s x

let fold' s n = fold s (const n)

let get_or_else x = fold' id x

let to_right e = fold' Either._Right (Either.Left e)
let to_left e = fold' Either._Left (Either.Right e)

let iter f = fold' f ()
