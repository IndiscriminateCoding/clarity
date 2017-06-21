open Fn

module Arr = struct
  type 'a t = 'a array

  let set = Array.set
  let get = Array.get
  let len = Array.length
  let make = Array.make
  let copy = Array.copy
  let iter = Array.iter
  let foldl = Array.fold_left
  let foldr' f a x = Array.fold_right f x a

  let foldr : type a x . (x -> (unit -> a) -> a) -> (unit -> a) -> x t -> a =
    fun f a x ->
      let rec loop (a : unit -> a) idx =
        if idx = len x
        then a ()
        else f (get x idx) (fun () -> loop a (idx + 1)) in
      loop a 0

  let copy_to_from : 'a array -> int -> 'a array -> int -> int -> unit =
    fun dst dst_off src src_off len ->
      for i = 0 to len - 1 do
        set dst (i + dst_off) (get src (i + src_off))
      done
end

(* Array pair as a single data-type *)
module AP = struct
  type 'a t = 'a array * 'a array

  let len (l, r) = Arr.len l + Arr.len r

  let get (l, r) i =
    if i < Arr.len l
    then Arr.get l i
    else Arr.get r (i - Arr.len l)

  let set (l, r) i x =
    if i < Arr.len l
    then Arr.set l i x
    else Arr.set r (i - Arr.len l) x

  let fold f a (l, r) =
    Arr.foldl f (Arr.foldl f a l) r
end

(* ceiling of a division *)
let ceil_div a b = 1 + (a - 1) / b

module Internal = struct
  let _BRANCHING = 32
  let _BITS = 5
  let _EXTRA_STEPS = 2
  let _SKIP_SIZE = _BRANCHING - ceil_div _EXTRA_STEPS 2

  type 'a t =
    | Leaf of 'a array
    | R_node of int array * 'a t array
    | B_node of 'a t array

  (* Depth of a tree *)
  let depth x =
    let rec loop : type a . int -> a t -> int =
      fun a -> function
      | Leaf _ -> a
      | R_node (_, n) | B_node n ->
        assert (Arr.len n > 0);
        loop (a + 1) (Arr.get n 0) in
    loop 0 x

  let rec length = function
    | Leaf x -> Arr.len x
    | R_node (i, v) ->
      assert (Arr.len i = Arr.len v);
      Arr.get i (Arr.len i - 1)
    | B_node v as node ->
      assert (Arr.len v > 0);
      let d = depth node in
      let item_sz = 1 lsl (d * _BITS) in
      item_sz * (Arr.len v - 1) + length (Arr.get v (Arr.len v - 1))

  let update_lengths = function
    | Leaf _ | B_node _ -> ()
    | R_node (is, vs) ->
      assert (Arr.len is = Arr.len vs);
      let sum = ref 0 in
      for i = 0 to Arr.len vs - 1 do
        sum := !sum + length (Arr.get vs i);
        Arr.set is i !sum
      done

  let mk_rnode arr =
    let res = R_node (Arr.make (Arr.len arr) 0, arr) in
    update_lengths res;
    res

  (* returns index of a slot and new 'index' value *)
  let rr_search : int array -> int -> int -> int * int =
    fun sizes depth idx ->
      assert (Arr.len sizes > 0);
      assert (idx <= Arr.get sizes (Arr.len sizes - 1));
      let start = idx lsr (_BITS * depth) in
      assert (start < Arr.len sizes);
      let rec loop n =
        assert (n < Arr.len sizes);
        let sz = Arr.get sizes n in
        if sz > idx
        then n
        else loop (n + 1) in
      let slot = loop start in
      let new_idx =
        if slot = 0
        then idx
        else idx - Arr.get sizes (slot - 1) in
      slot, new_idx

  let radix_search : int -> int -> int * int =
    fun depth idx ->
      let shift = _BITS * depth in
      let slot = idx lsr shift in
      slot, idx - slot lsl shift
end

include Internal

(* Max node count allowed to contain such subnodes *)
let max_nodes_allowed subnodes =
  _EXTRA_STEPS + (subnodes - 1) / _BRANCHING + 1

let empty = Leaf [||]

let get_leaf = function
  | Leaf x -> x
  | _ -> assert false

let get_rnode = function
  | R_node (i, v) -> i, v
  | B_node v ->
    let sizes = Arr.make (Arr.len v) 0 in
    update_lengths (R_node (sizes, v));
    sizes, v
  | _ -> assert false

let get_bnode = function
  | B_node x -> x
  | _ -> assert false

let node_len : type a . a t -> int =
  function
  | Leaf x -> Arr.len x
  | R_node (_, x) | B_node x -> Arr.len x

(* Concatenation is complex; move it to separate module *)
module Concatenation = struct
  (* Compute lengths of _new_ subnodes and store it in a provided array pair
   * Shared nodes will have values unchanged
   *)
  let assign_subnode_lengths : type a . a t AP.t -> int AP.t -> unit =
    fun src dst ->
    let dst_idx = ref 0 in
    let src_idx = ref 0 in
    let size = ref 0 in
    let not_done =
      let need = AP.len src - AP.len dst in
      fun () ->
        let need = if !size = 0 then need else need + 1 in
        let diff = !src_idx - !dst_idx in
        assert (diff <= need);
        diff <> need in
    let get () =
      assert (!src_idx < AP.len src);
      let res = AP.get src !src_idx in
      incr src_idx;
      res in
    let push (i : int) =
      assert (!dst_idx < AP.len dst);
      match () with
      | _ when i >= _SKIP_SIZE && !size = 0 ->
        (* AP.set dst !dst_idx i; *)
        incr dst_idx
      | _ when !size + i > _BRANCHING && !size < _SKIP_SIZE ->
        AP.set dst !dst_idx _BRANCHING;
        incr dst_idx;
        size := !size + i - _BRANCHING
      | _ when !size + i > _BRANCHING ->
        AP.set dst !dst_idx !size;
        incr dst_idx;
        size := i
      | _ ->
        size := !size + i in
    while not_done () do
      push (node_len (get ()))
    done;
    if !size <> 0
      then begin
        AP.set dst !dst_idx !size;
        incr dst_idx end (* ;
    for i = 0 to AP.len dst - !dst_idx - 1 do
      let node = AP.get src (!src_idx + i) in
      AP.set dst (!dst_idx + i)(node_len node)
    done *)

  (* copy subnode data according to 'lengths' array *)
  let copy_subnode_data : type a . a t AP.t -> a t AP.t -> int AP.t -> unit =
    fun (lnodes, _ as src_v) dst_v lengths ->
    let
        (copy_to_node : a t -> int -> a t -> int -> int -> unit),
        (new_node : int -> a t) =
      assert (Arr.len lnodes > 0);
      match Arr.get lnodes 0 with
      | Leaf x ->
        assert (Arr.len x > 0);
        (fun dst dst_off src ->
          Arr.copy_to_from (get_leaf dst) dst_off (get_leaf src))
        , fun n -> Leaf (Arr.make n (Arr.get x 0))
      | node ->
        let l, x = get_rnode node in
        assert (Arr.len l = Arr.len x);
        assert (Arr.len x > 0);
        (fun dst dst_off src ->
          let dst_ = snd (get_rnode dst) in
          let src_ = snd (get_rnode src) in
          Arr.copy_to_from dst_ dst_off src_)
        , fun n ->
          R_node (Arr.make n (Arr.get l 0), Arr.make n (Arr.get x 0)) in
    let src_idx = ref 0 in
    let src_off = ref 0 in
    for i = 0 to AP.len lengths - 1 do
      let new_len = AP.get lengths i in
      if new_len = 0 then (
        (* shared chunk *)
        assert (!src_off = 0);
        AP.set dst_v i (AP.get src_v !src_idx);
        incr src_idx;
      ) else (
        let nn = new_node new_len in
        let rec loop = function
        | n when n = new_len -> ()
        | nn_off ->
          let src_node = AP.get src_v !src_idx in
          let sz = min (new_len - nn_off) (node_len src_node - !src_off) in
          copy_to_node nn nn_off src_node !src_off sz;
          src_off := !src_off + sz;
          if !src_off = node_len src_node
            then (src_off := 0; incr src_idx);
          loop (nn_off + sz) in
        loop 0;
        AP.set dst_v i nn
      )
    done

  (* compute indices after copying data *)
  let compute_indices : type a . a t array -> int array -> int array -> unit =
    fun xv xi lengths ->
    assert (Arr.len xv = Arr.len xi);
    assert (Arr.len xv = Arr.len lengths);
    let sum = ref 0 in
    for i = 0 to Arr.len lengths - 1 do
      let len = Arr.get lengths i in
      let node = Arr.get xv i in
      if len <> 0 then update_lengths node;
      sum := !sum + length node;
      Arr.set xi i !sum
    done

  (* Merge two nodes of the same depth *)
  let merge : 'a t -> 'a t -> 'a t =
    fun l r ->
    let _, lv = get_rnode l in
    let _, rv = get_rnode r in
    assert (Arr.len lv > 0);
    let nodes = Arr.len lv + Arr.len rv in
    let subnodes =
      let sum a x = a + node_len x in
      Arr.foldl sum (Arr.foldl sum 0 lv) rv in
    let max_nodes = max_nodes_allowed subnodes in
    if max_nodes >= nodes
    then (* no balancing required *)
      mk_rnode (if node_len r = 0 then [| l |] else [| l; r |])
    else begin
      let len_l, len_r =
        if max_nodes <= _BRANCHING
        then max_nodes, 0
        else _BRANCHING, max_nodes - _BRANCHING in
      let length_l = Arr.make len_l 0 in
      let length_r = Arr.make len_r 0 in
      let lengths = length_l, length_r in
      assign_subnode_lengths (lv, rv) lengths;
      let node_l = Arr.make len_l empty in
      let node_r = Arr.make len_r empty in
      let new_nodes = node_l, node_r in
      copy_subnode_data (lv, rv) new_nodes lengths;
      compute_indices node_l length_l length_l;
      compute_indices node_r length_r length_r;
      let lres = R_node (fst lengths, fst new_nodes) in
      let res =
        if len_r = 0
        then [| lres |]
        else
          let rres = R_node (snd lengths, snd new_nodes) in
          [| lres; rres |] in
      mk_rnode res
    end

  let leftmost = function
    | R_node (_, x) | B_node x ->
      Arr.get x 0
    | Leaf _ -> assert false

  let rightmost = function
    | R_node (_, x) | B_node x ->
      Arr.get x (Arr.len x - 1)
    | Leaf _ -> assert false

  (* append vectors of the same depth and return vector that is one-level
     greater *)
  let rec append_same : type a . a t -> a t -> int -> a t =
    fun l r n ->
    assert (depth l = n);
    assert (depth r = n);
    match n with
    | 0 ->
      assert (node_len l > 0);
      assert (node_len r > 0);
      (match l, r with
      | Leaf _, Leaf _ -> ()
      | _ -> assert false);
      mk_rnode [| l; r |]
    | 1 -> merge l r
    | n ->
      let li, lv = get_rnode l in
      let ri, rv = get_rnode r in
      assert (Arr.len lv > 0);
      assert (Arr.len rv > 0);
      let intermediate = append_same (rightmost l) (leftmost r) (n - 1) in
      let ii, iv = get_rnode intermediate in
      let overall = node_len l + node_len r - 2 + node_len intermediate in
      let ll, lr =
        if overall > _BRANCHING
        then _BRANCHING, overall - _BRANCHING
        else overall, 0 in
      let lnode = Arr.make ll empty in
      let rnode = Arr.make lr empty in
      let ap = lnode, rnode in
      let idx = ref 0 in
      for i = 0 to Arr.len lv - 2 do
        AP.set ap !idx (Arr.get lv i);
        incr idx
      done;
      for i = 0 to Arr.len iv - 1 do
        AP.set ap !idx (Arr.get iv i);
        incr idx
      done;
      for i = 1 to Arr.len rv - 1 do
        AP.set ap !idx (Arr.get rv i);
        incr idx
      done;
      let l = R_node (Arr.make ll 0, lnode) in
      let r = R_node (Arr.make lr 0, rnode) in
      update_lengths l;
      update_lengths r;
      merge l r

  let append : type a . a t -> a t -> a t =
    fun l r ->
    match l, r with
    | Leaf [||], x | x, Leaf [||] -> x
    | _ ->
      let dl = depth l in
      let dr = depth r in
      let rec add_layers x = function
      | 0 -> x
      | n -> add_layers (mk_rnode [| x |]) (n - 1) in
      let res =
        match () with
        | _ when dl = dr -> append_same l r dl
        | _ when dl < dr -> append_same (add_layers l (dr - dl)) r dr
        | _ (* when dl > dr *) -> append_same l (add_layers r (dl - dr)) dl in
      match res with
      | R_node (is, vs) when Arr.len vs = 1 ->
        Arr.get vs 0
      | _ -> res
end

let append = Concatenation.append

exception Out_of_bounds of { index : int; size : int }

let out_of_bounds index size =
  raise (Out_of_bounds { index; size })

let get : type a . a t -> int -> a =
  fun n i ->
    let ns = length n in
    if ns - 1 < i || i < 0 then out_of_bounds i ns;
    let rec loop n i = function
    | 0 -> Arr.get (get_leaf n) i
    | d ->
      begin match n with
      | Leaf _ -> assert false
      | R_node (is, vs) ->
        let slot, new_i = rr_search is d i in
        loop (Arr.get vs slot) new_i (d - 1)
      | B_node vs ->
        let slot, new_i = radix_search d i in
        loop (Arr.get vs slot) new_i (d - 1) end in
    loop n i (depth n)

let update : type a . a t -> int -> a -> a t =
  fun n i x ->
    let ns = length n in
    if ns - 1 < i || i < 0 then out_of_bounds i ns;
    let rec loop n i = function
    | 0 ->
      let res = Arr.copy (get_leaf n) in
      Arr.set res i x;
      Leaf res
    | d ->
      begin match n with
      | Leaf _ -> assert false
      | R_node (is, vs) ->
        let slot, new_i = rr_search is d i in
        let upd = loop (Arr.get vs slot) new_i (d - 1) in
        let res = Arr.copy vs in
        Arr.set res slot upd;
        R_node (is, res)
      | B_node vs ->
        let slot, new_i = radix_search d i in
        let upd = loop (Arr.get vs slot) new_i (d - 1) in
        let res = Arr.copy vs in
        Arr.set res slot upd;
        B_node res end in
    loop n i (depth n)

let split_at : type a . a t -> int -> a t * a t =
  fun n i ->
    let ns = length n in
    if ns < i || i < 0 then out_of_bounds i ns;
    let rec loop n i = function
    | 0 ->
      begin match i with
      | 0 -> None, Some n
      | i when node_len n = i -> Some n, None
      | i ->
        let a = get_leaf n in
        let al = Arr.make i (Arr.get a 0) in
        let ar = Arr.make (Arr.len a - i) (Arr.get a i) in
        Arr.copy_to_from al 1 a 1       (i - 1);
        Arr.copy_to_from ar 1 a (i + 1) (Arr.len a - i - 1);
        Some (Leaf al), Some (Leaf ar) end
    | d ->
      begin match n with
      | Leaf _ -> assert false
      | node when i = 0 -> None, Some node
      | node when i = length node -> Some node, None
      | node ->
        let is, vs = get_rnode node in
        assert (Arr.len is = Arr.len vs);
        assert (Arr.len is > 0);
        let slot, new_i = rr_search is d i in
        let l, r = loop (Arr.get vs slot) new_i (d - 1) in
        let len_l = if l = None then slot else slot + 1 in
        let len_r = Arr.len vs - if r = None then slot + 1 else slot in
        let nl =
          Some (
            let lv = Arr.make len_l (Arr.get vs 0) in
            for j = 1 to slot - 1 do
              Arr.set lv j (Arr.get vs j)
            done;
            begin match l with
            | None -> ()
            | Some x -> Arr.set lv slot x end;
            mk_rnode lv
          ) in
        let nr =
          Some (
            let rv = Arr.make len_r (Arr.get vs 0) in
            begin match r with
            | None ->
              for j = 0 to len_r - 1 do
                Arr.set rv j (Arr.get vs (j + slot))
              done
            | Some x ->
              Arr.set rv 0 x;
              for j = 1 to len_r - 1 do
                Arr.set rv j (Arr.get vs (j + slot))
              done end;
            mk_rnode rv
          ) in
          nl, nr end in
    let l, r = loop n i (depth n) in
    let get = function
    | Some x -> x
    | None -> empty in
    get l, get r

let take : type a . a t -> int -> a t =
  fun n i ->
    let sz = length n in
    fst (split_at n (if i > sz then sz else i))

let drop : type a . a t -> int -> a t =
  fun n i ->
    let sz = length n in
    snd (split_at n (if i > sz then sz else i))

let rec iter f = function
  | Leaf x -> Arr.iter f x
  | R_node (_, x) | B_node x -> Arr.iter (iter f) x

let make_pb : unit -> (('a -> unit) * (unit -> 'a t)) =
  fun () ->
    let realloc_array a n =
      assert (Arr.len a > n);
      let res = Arr.make n (Arr.get a 0) in
      for i = 1 to n - 1 do
        Arr.set res i (Arr.get a i)
      done;
      res in
    let realloc = function
    | sz, n when sz = _BRANCHING -> n
    | sz, Leaf x -> Leaf (realloc_array x sz)
    | sz, B_node x -> B_node (realloc_array x sz)
    | _ -> assert false in
    let rec push_node n = function
    | [] -> [ 1, B_node (Arr.make _BRANCHING n) ]
    | (sz, node) :: t when sz < _BRANCHING ->
      Arr.set (get_bnode node) sz n;
      (sz + 1, node) :: t
    | (sz, node) :: t ->
      assert (sz = _BRANCHING);
      (1, B_node (Arr.make _BRANCHING n)) :: push_node node t in
    let push_elem e = function
    | [] -> [ 1, Leaf (Arr.make _BRANCHING e) ]
    | (sz, node) :: t when sz < _BRANCHING ->
      Arr.set (get_leaf node) sz e;
      (sz + 1, node) :: t
    | (sz, node) :: t ->
      assert (sz = _BRANCHING);
      (1, Leaf (Arr.make _BRANCHING e)) :: push_node node t in
    let res = ref [] in
    let push x = res := push_elem x !res in
    let rec build = function
    | [] -> empty
    | [ x ] -> realloc x
    | h :: t -> build (push_node (realloc h) t) in
    push, fun () -> build !res

let init : type a . int -> (int -> a) -> a t =
  fun l f ->
    let push, build = make_pb () in
    for i = 0 to l do
      push (f i)
    done;
    build ()

include Monad.Make(struct
  type nonrec 'a t = 'a t

  let pure x = Leaf [| x |]

  let map : type a b . (a -> b) -> a t -> b t =
    fun f x ->
      let push, build = make_pb () in
      iter (compose push f) x;
      build ()

  let bind f x =
    let res = ref empty in
    iter (fun x -> res := append !res (f x)) x;
    !res
  let ap f x =
    if f = empty
    then empty
    else let x = x () in
      flip bind f @@ fun f ->
      flip bind x @@ fun x ->
      pure (f x)
end)

include Foldable.Make(struct
  type nonrec 'a t = 'a t

  let rec foldl f a = function
  | Leaf x -> Arr.foldl f a x
  | R_node (_, x) | B_node x -> Arr.foldl (foldl f) a x

  let rec foldr f a = function
  | Leaf x -> Arr.foldr f a x
  | R_node (_, x) | B_node x -> Arr.foldr (fun x a -> foldr f a x) a x
end)

let rec foldr' f a = function
  | Leaf x -> Arr.foldr' f a x
  | R_node (_, x) | B_node x -> Arr.foldr' (fun x a -> foldr' f a x) a x

let to_list x = foldr' Clarity_list._Cons [] x
let of_list x =
  let push, build = make_pb () in
  Clarity_list.iter push x;
  build ()

include Align.Make(struct
  type nonrec 'a t = 'a t

  let align_with f a b =
    let open These in
    let la = length a in
    let lb = length b in
    let push, build = make_pb () in
    for i = 0 to min la lb - 1 do
      push (f (_Both (get a i) (get b i)))
    done;
    if la > lb
    then for i = lb to la - 1 do
        push @@ f @@ Left (get a i)
    done else if la < lb
    then for i = la to lb - 1 do
        push @@ f @@ Right (get b i)
    done;
    build ()
end)

module A3 (A : Applicative.Basic3) = Traversable.Make3(struct
  type nonrec 'a t = 'a t
  type ('u, 'v, 'a) f = ('u, 'v, 'a) A.t

  module Ap = Applicative.Make3(A)

  let traverse f x =
    let cf x l =
      let open! Ap in
      ap (map (fun h t -> h :: t) (f x)) l in
    let ls = foldr cf (defer Ap.pure []) x in
    Ap.map of_list ls

  let traverse_ f =
    foldr (compose Ap.discard_left f) (defer Ap.pure ())
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

