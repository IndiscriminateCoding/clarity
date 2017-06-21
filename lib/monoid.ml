open Fn

module type S = sig
  include Semigroup.S
  val zero : t
end

module Int = struct
  module Min = struct
    type t = int
    let append = min
    let zero = min_int
  end
  module Max = struct
    type t = int
    let append = max
    let zero = max_int
  end
  module Sum = struct
    type t = int
    let append = (+)
    let zero = 0
  end
  module Product = struct
    type t = int
    let append = ( * )
    let zero = 1
  end
end

module All = struct
  type t = bool
  let append = (&&)
  let zero = true
end

module Any = struct
  type t = bool
  let append = (||)
  let zero = false
end

module Dual (M : S) = struct
  type t = M.t
  let append = flip M.append
  let zero = M.zero
end

module Endo (T : sig type t end) = struct
  type t = T.t -> T.t
  let append = compose
  let zero = id
end

module Pair (M1 : S)(M2 : S) = struct
  type t = M1.t * M2.t
  let append (a, b) (x, y) = M1.append a x, M2.append b y
  let zero = M1.zero, M2.zero
end

module Opt (S : Semigroup.S) = struct
  type t = S.t option
  let append = curry
    (function
    | Some a, Some b -> Some (S.append a b)
    | _, (Some _ as x) | (Some _ as x), _ -> x
    | _ -> None)
  let zero = None
end

