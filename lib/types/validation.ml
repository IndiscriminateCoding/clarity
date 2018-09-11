module Make (S : Semigroup.S) = struct
  open Either

  type 'a t = (S.t, 'a) Either.t

  include Applicative.Make(struct
    type nonrec 'a t = 'a t

    let pure = pure
    let map = map
    let ap f x =
      match f, x () with
      | Right f, Right x -> Right (f x)
      | Left a, Left b -> Left (S.append a b)
      | Left _ as l, _ | _, (Left _ as l) -> l
  end)

  let map_errors f = bimap f Fn.id
  let fail = _Left
  let fold = fold
  let maybe_errors = maybe_left
  let maybe_result = maybe_right
  external to_either : 'a t -> (S.t, 'a) Either.t = "%identity"
  external of_either : (S.t, 'a) Either.t -> 'a t = "%identity"
end

