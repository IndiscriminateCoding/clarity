module Make(M : Monoid.S) = struct
  type 'a t = M.t * 'a

  include Monad.Make(struct
    type nonrec 'a t = 'a t

    let map f (w, x) = (w, f x)
    let pure x = (M.zero, x)
    let bind f (w, x) =
      let (w', x) = f x in
      (M.append w w', x)
    let ap (fw, fv) x =
      let xw, xv = x () in
      M.append fw xw, fv xv
  end)

  external run : 'a t -> (M.t * 'a) = "%identity"
  let writer : 'a -> M.t -> 'a t =
    fun a w -> w, a

  let tell : M.t -> unit t =
    fun w -> w, ()
  let listen : 'a t -> ('a * M.t) t =
    fun (w, a) -> (w, (a, w))
  let censor : (M.t -> M.t) -> 'a t -> 'a t =
    fun f (w, x) -> (f w, x)

  let zip_with f (w1, x1) (w2, x2) = (M.append w1 w2, f x1 x2)
  let zip a b = zip_with (fun a b -> a, b) a b
end

