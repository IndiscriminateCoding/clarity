include (struct
  type 'a t = 'a
  let map f = f
  let pure x = x
  let ap f x = f (x ())
  let bind f = f
end : Monad.Basic with type 'a t = 'a)

