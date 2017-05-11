open Fn

type 'a t = 'a list

let _Cons h t = h :: t
let _Nil = []

let length = List.length
let rev = List.rev
let rev_map = List.rev_map
let sort ?(cmp = compare) = List.stable_sort cmp
let append l = List.rev_append (rev l)
let rev_mapi f l =
  let i = ref (-1) in
  rev_map (fun x -> incr i; f !i x) l
let mapi f = compose (rev_mapi f) rev
let filter = List.filter
let rec find p = function
  | h :: _ when p h -> Some h
  | _ :: t -> find p t
  | [] -> None

include Monad.Make(struct
  type nonrec 'a t = 'a t

  let map f = compose (rev_map f) rev
  let pure x = [ x ]
  let bind f x =
    let rec loop a = function
      | [] -> a
      | h :: t -> loop (compose a (append (f h))) t
  in loop id x []
  let ap f x =
    flip bind f @@ fun f ->
    flip bind (x ()) @@ fun x ->
    pure (f x)
end)

include Align.Make(struct
  type nonrec 'a t = 'a t

  let align_with f a b =
    let open These in
    let rec loop a = function
      | [], [] -> a
      | [] as l, x :: r -> loop (f (Right x) :: a) (l, r)
      | x :: l, ([] as r) -> loop (f (Left x) :: a) (l, r)
      | x :: l, y :: r -> loop (f (_Both x y) :: a) (l, r) in
    rev (loop [] (a, b))
end)

include Foldable.Make(struct
  type nonrec 'a t = 'a t

  let rec foldl f a = function
    | [] -> a
    | h :: t -> foldl f (f a h) t
  let rec foldr f a = function
    | [] -> a
    | h :: t -> f h (defer (foldr f a) t)
end)

module WithA3(A : Applicative.Basic3) = Traversable.Make3(struct
  type nonrec 'a t = 'a t
  type ('u, 'v, 'a) f = ('u, 'v, 'a) A.t

  module Ap = Applicative.Make3(A)

  let traverse f =
    let cf x l =
      let open! Ap in
      ap (map _Cons (f x)) l in
    foldr cf (Ap.pure [])
  let traverse_ f =
    foldr (compose Ap.discard_left f) (Ap.pure ())
end)

module WithA2(A : Applicative.Basic2) = WithA3(struct
  type (_, 'p, 'a) t = ('p, 'a) A.t
  include (A : Applicative.Basic2 with type ('p, 'a) t := ('p, 'a) A.t)
end)

module WithA(A : Applicative.Basic) = WithA2(struct
  type (_, 'a) t = 'a A.t
  include (A : Applicative.Basic with type 'a t := 'a A.t)
end)

module WithM3(M : Monad.Basic3) = struct
  include WithA3(M)

  let foldr_m f a l =
    let g k x z = M.bind k (f x z) in
    foldl g M.pure l a

  let foldl_m f a l =
    let g x k z = M.bind (fun x -> k () x) (f z x) in
    foldr g M.pure l a
end

module WithM2(M : Monad.Basic2) = WithM3(struct
  type (_, 'p, 'a) t = ('p, 'a) M.t
  include (M : Monad.Basic2 with type ('p, 'a) t := ('p, 'a) M.t)
end)

module WithM(M : Monad.Basic) = WithM2(struct
  type (_, 'a) t = 'a M.t
  include (M : Monad.Basic with type 'a t := 'a M.t)
end)

