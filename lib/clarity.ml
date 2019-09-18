(* Re-export shadowed stdlib modules *)
module Caml = struct
  module List = List
end

module Align = Align
module Applicative = Applicative
module Either = Either
module Fn = Fn
module Foldable = Foldable
module Functor = Functor
module Id = Id
module List = Clarity_list
module Monad = Monad
module Monoid = Monoid
module Option = Option
module Semigroup = Semigroup
module These = These
module Traversable = Traversable
module Validation = Validation
module Vector = Vector
module Void = Void

(* This modules is exported for testing purposes only *)
module Internal = struct
  module Vector = Vector_internal
end

