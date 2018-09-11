module type S = sig
  type t
  val append : t -> t -> t
end

module First (T : sig type t end) = struct
  type t = T.t
  let append x _ = x
end

module Last (T : sig type t end) = struct
  type t = T.t
  let append _ x = x
end

