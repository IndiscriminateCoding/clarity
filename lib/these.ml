type ('l, 'r) t =
  | Left of 'l
  | Right of 'r
  | Both of 'l * 'r

let _Left x = Left x
let _Right x = Right x
let _Both l r = Both (l, r)

let bimap lf rf = function
  | Left l -> Left (lf l)
  | Right r -> Right (rf r)
  | Both (l, r) -> Both (lf l, rf r)

module Make(S : Semigroup.S) = struct
  type nonrec 'a t = (S.t, 'a) t
  include Monad.Make(struct
    type nonrec 'a t = 'a t

    let pure x = Right x
    let bind f = function
    | Left l -> Left l
    | Right r -> f r
    | Both (l, x) ->
      begin match f x with
      | Left l' -> Left (S.append l l')
      | Right r -> Both (l, r)
      | Both (l', r) -> Both (S.append l l', r)
      end

    let ap f x =
      match f with
      | Left l -> Left l
      | Right f ->
        begin match x () with
        | Left b -> Left b
        | Right x -> Right (f x)
        | Both (l, x) -> Both (l, f x)
        end
      | Both (a, f) ->
        begin match x () with
        | Left b -> Left (S.append a b)
        | Right x -> Both (a, f x)
        | Both (b, x) -> Both (S.append a b, f x)
        end

    let map f = function
    | Left l -> Left l
    | Right r -> Right (f r)
    | Both (l, r) -> Both (l, f r)
  end)
end

let fold lf rf bf = function
  | Left l -> lf l
  | Right r -> rf r
  | Both (l, r) -> bf l r

let swap = function
  | Left a -> Right a
  | Right a -> Left a
  | Both (a, b) -> Both (b, a)

let maybe_left = function
  | Left x -> Some x
  | _ -> None

let maybe_right = function
  | Right x -> Some x
  | _ -> None

let maybe_both = function
  | Both (l, r) -> Some (l, r)
  | _ -> None

