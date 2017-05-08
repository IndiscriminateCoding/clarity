let curry f a b = f (a, b)
let uncurry f (a, b) = f a b
external id : 'a -> 'a = "%identity"
let const a _ = a
let compose f g x = f (g x)
let flip f a b = f b a
let defer f x = fun _ -> f x

