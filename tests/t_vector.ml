open Printf

open Clarity
open Vector
open Vector.Internal

module A = Array

let rec check_bnode check_size = function
  | B_node vs ->
    if check_size
      then assert (A.length vs = _BRANCHING);
    for i = 0 to A.length vs - 2 do
      check_bnode true (A.get vs i)
    done;
    check_bnode false (A.get vs (A.length vs - 1))
  | Leaf x ->
    if check_size
      then assert (A.length x = _BRANCHING)
  | _ -> assert false

let rec check_indices = function
  | Leaf _ -> ()
  | B_node _ as bnode -> check_bnode false bnode
  | R_node (is, vs) ->
    assert (A.length is = A.length vs);
    assert (A.length is <= _BRANCHING);
    assert (A.length is > 0);
    let size = ref 0 in
    for i = 0 to A.length is - 1 do
      let node = A.get vs i in
      size := !size + length node;
      assert (A.get is i = !size);
      A.iter check_indices vs
    done

module Concat = struct
  let to_list i v =
    let res = ref i in
    iter (fun x -> res := x :: !res) v;
    !res

  let mk =
    let n = ref 0 in
    fun l ->
      Leaf (A.init l (fun _ -> incr n; !n))
  let mk n = mk_rnode (A.init n (fun _ -> mk 25))
  let mk n = mk_rnode (A.init n (fun _ -> mk 27))
  let src_a = mk 11
  let src_b = mk 15

  let src_a = append src_a src_a
  let src_b = append src_b src_b
  let src_a = map (fun x -> x) src_a
  let src_b = append src_b src_a
  let src_a = append src_a src_b
  let src_b = map (fun x -> x) src_b
  let src_a = append src_b src_a
  let src_a = append src_a src_a

  let dst = append src_a src_b

  ;; check_indices src_a
  ;; check_indices src_b
  ;; check_indices dst

  let dst_lst = to_list [] dst
  ;; assert (to_list (to_list [] src_a) src_b = dst_lst)
  ;; assert (List.length dst_lst = length dst)
  ;; printf "Vector.Concat OK\n"
end

module Concat2 = struct
  let mk =
    let n = ref 0 in
    fun l ->
      Leaf (A.init l (fun _ -> incr n; !n))

  let vals = A.init 31 (fun i -> i + 1)

  ;;
  for a = 0 to A.length vals - 1 do
  for b = 0 to A.length vals - 1 do
  for c = 0 to A.length vals - 1 do
    let a = A.get vals a in
    let b = A.get vals b in
    let c = A.get vals c in

    let mk n = mk_rnode (A.init n (fun _ -> mk c)) in
    let src_a = mk b in
    let src_b = mk a in
    let src_a = append src_a src_b in
    let src_b = append src_b src_a in
    let src_a = map (fun x -> x) src_a in
    let src_a = append src_a src_b in
    let src_b = append src_b src_a in
    let src_b = map (fun x -> x) src_b in
    let src_a = append src_a src_b in
    let src_b = append src_b src_a in
    check_indices src_b;
    flush stdout
  done
  done
  done

  ;; printf "Vector.Concat2 OK\n"
end

module RR_search = struct
  (* rr_search test *)
  let sizes = [| 31; 62; 94 |]
  let depth = 1
  let idx = 91
  let slot, new_idx = rr_search sizes depth idx

  ;; assert (slot = 2)
  ;; assert (new_idx = 29)

  ;; printf "Vector.RR_search OK\n"
end

module Get = struct
  let mk =
    let n = ref (-1) in
    fun l ->
      Leaf (A.init l (fun _ -> incr n; !n))
  let mk n = mk_rnode (A.init n (fun _ -> mk 32))
  let mk n = mk_rnode (A.init n (fun _ -> mk 30))
  let mk n = mk_rnode (A.init n (fun _ -> mk 7))
  let src = mk 27

  let idx = ref 0
  ;; iter (fun x -> assert (x = get src !idx); incr idx) src

  ;; printf "Vector.Get OK\n"
end

module Update = struct
  let mk =
    let n = ref (-1) in
    fun l ->
      Leaf (A.init l (fun _ -> incr n; !n))
  let mk n = mk_rnode (A.init n (fun _ -> mk 3))
  let mk n = mk_rnode (A.init n (fun _ -> mk 2))
  let mk n = mk_rnode (A.init n (fun _ -> mk 4))
  let src = mk 3

  let src = update src 10 10000
  let upd_val = -123
  ;; for i = 0 to length src - 1 do
    assert (get (update src i upd_val) i = upd_val)
  done

  ;; printf "Vector.Update OK\n"
end

module Map = struct
  (* map test *)
  let mk =
    let n = ref (-1) in
    fun l ->
      Leaf (A.init l (fun _ -> incr n; !n))
  let mk n = mk_rnode (A.init n (fun _ -> mk 4))
  let mk n = mk_rnode (A.init n (fun _ -> mk 8))
  let mk n = mk_rnode (A.init n (fun _ -> mk 4))
  let mk n = mk_rnode (A.init n (fun _ -> mk 8))

  let src = mk 1
  let dst = map (fun x -> x * 100000) src
  ;; assert (length src = _BRANCHING lsl _BITS)
  ;; assert (length dst = _BRANCHING lsl _BITS)

  let idx = ref 0
  ;; iter (fun x -> assert (x = 100000 * get src !idx); incr idx) dst
  ;; check_bnode true dst
  ;; assert (depth dst = 1)

  ;; printf "Vector.Map OK\n"
end

module Split_at = struct
  let mk =
    let n = ref (-1) in
    fun l ->
      Leaf (A.init l (fun _ -> incr n; !n))
  let mk n = mk_rnode (A.init n (fun _ -> mk 5))
  let mk n = mk_rnode (A.init n (fun _ -> mk 4))
  let mk n = mk_rnode (A.init n (fun _ -> mk 8))
  let mk n = mk_rnode (A.init n (fun _ -> mk 7))

  let src = mk 1

  ;; for n = 0 to length src do
    let l, r = split_at src n in
    let src2 = append l r in
    let l, r = split_at src2 n in
    assert (l = take src2 n);
    assert (r = drop src2 n);
    assert (length l + length r = length src);

    let idx = ref 0 in
    iter (fun x -> assert (x = get src !idx); incr idx) l;
    iter (fun x -> assert (x = get src !idx); incr idx) r;
    check_indices l;
    check_indices r
  done

  ;; printf "Vector.Split_at OK\n"
end

module Align = struct
  let to_list v =
    let res = ref [] in
    iter (fun x -> res := x :: !res) v;
    !res

  open These
  let lefts = foldl
    (fun a -> function Left l | Both (l, _) -> l :: a | _ -> a) []
  let rights = foldl
    (fun a -> function Right r | Both (_, r) -> r :: a | _ -> a) []

  let mk =
    let n = ref (-1) in
    fun l ->
      Leaf (A.init l (fun _ -> incr n; !n))
  let mk n = mk_rnode (A.init n (fun _ -> mk 5))
  let mk n = mk_rnode (A.init n (fun _ -> mk 4))
  let mk n = mk_rnode (A.init n (fun _ -> mk 8))

  let a = mk 5
  let b = mk 3
  ;; assert (length a > length b)

  let c = align a b

  let idx = ref 0
  ;; assert (to_list a = lefts c)
  ;; assert (to_list b = rights c)

  ;; printf "Vector.Align OK\n"
end

