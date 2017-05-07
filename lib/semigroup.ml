module type S = sig
  type t
  val append : t -> t -> t
end

type 'a t = 'a -> 'a -> 'a

let pack : type a . a t -> (module S with type t = a) =
  fun x ->
    (module struct
      type t = a
      let append = x
    end)
let unpack : type a . (module S with type t = a) -> a t =
  fun m ->
    let module M = (val m) in
    M.append

let first x _ = x
let last _ x = x

