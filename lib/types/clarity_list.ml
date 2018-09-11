open Fn

type 'a t = 'a list

let _Cons h t = h :: t
let _Nil = []

let iter = List.iter
let rev = List.rev
let rev_append = List.rev_append
let rev_map = List.rev_map
let rev_mapi f l =
  let i = ref (-1) in
  rev_map (fun x -> incr i; f !i x) l
let length = List.length
let sort ?(cmp = compare) = List.stable_sort cmp
let append l = rev_append (rev l)
let mapi f = rev_mapi f % rev
let filter = List.filter

let intersperse x =
  let rec prepend acc = function
  | [] -> acc
  | h :: t -> prepend (x :: h :: acc) t in
  function
  | [] -> []
  | h :: t -> h :: prepend [] (rev t)

include Monad.Make(struct
  type nonrec 'a t = 'a t

  let map f = rev_map f % rev
  let pure x = [ x ]
  let bind f x =
    let rec loop a = function
      | [] -> a
      | h :: t -> loop (a % append (f h)) t in
    loop id x []
  let ap f x =
    if f = []
    then []
    else let x = x () in
      flip bind f @@ fun f ->
      flip bind x @@ fun x ->
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
    | [] -> a ()
    | h :: t -> f h (defer (foldr f a) t)
end)

let foldr' f a x = foldl (flip f) a (rev x)

module A3 (A : Applicative.Basic3) = Traversable.Make3(struct
  type nonrec 'a t = 'a t
  type ('u, 'v, 'a) f = ('u, 'v, 'a) A.t

  module Ap = Applicative.Make3(A)

  let traverse f =
    let cf x l =
      let open! Ap in
      ap (map _Cons (f x)) l in
    foldr cf (defer Ap.pure [])
  let traverse_ f =
    foldr (Ap.discard_left % f) (defer Ap.pure ())
end)

module A2 (A : Applicative.Basic2) = A3(struct
  type (_, 'p, 'a) t = ('p, 'a) A.t
  include (A : Applicative.Basic2 with type ('p, 'a) t := ('p, 'a) A.t)
end)

module A (A : Applicative.Basic) = A2(struct
  type (_, 'a) t = 'a A.t
  include (A : Applicative.Basic with type 'a t := 'a A.t)
end)

module M3 (M : Monad.Basic3) = struct
  include A3(M)

  let foldr_m f a l =
    let g k x z = M.bind k (f x z) in
    foldl g M.pure l a

  let foldl_m f a l =
    let g x k z = M.bind (fun x -> k () x) (f z x) in
    foldr g (const M.pure) l a
end

module M2 (M : Monad.Basic2) = M3(struct
  type (_, 'p, 'a) t = ('p, 'a) M.t
  include (M : Monad.Basic2 with type ('p, 'a) t := ('p, 'a) M.t)
end)

module M (M : Monad.Basic) = M2(struct
  type (_, 'a) t = 'a M.t
  include (M : Monad.Basic with type 'a t := 'a M.t)
end)

