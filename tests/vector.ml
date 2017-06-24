open Printf

open Clarity
open Vector
open Vector.Internal

module A = Array

let ok s = printf "Vector.%s OK\n" s; flush stdout

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

  let dst_lst = to_list dst
  ;; assert (to_list src_a @ to_list src_b = dst_lst)
  ;; assert (List.length dst_lst = length dst)
  ;; ok "Concat"
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

  ;; ok "Concat2"
end

module RR_search = struct
  (* rr_search test *)
  let sizes = [| 31; 62; 94 |]
  let depth = 1
  let idx = 91
  let slot, new_idx = rr_search sizes depth idx

  ;; assert (slot = 2)
  ;; assert (new_idx = 29)

  ;; ok "RR_search"
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

  ;; try
    ignore @@ get src (length src);
    assert false
  with
  | Out_of_bounds _ -> ()
  | _ -> assert false

  ;; try
    ignore @@ get src (-1);
    assert false
  with
  | Out_of_bounds _ -> ()
  | _ -> assert false

  ;; ok "Get"
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

  ;; ok "Update"
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

  ;; ok "Map"
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

  ;; ignore @@ split_at src (length src + 1000)

  ;; ok "Split_at"
end

module Align = struct
  open These
  let lefts x =
    foldl
      (fun a -> function Left l | Both (l, _) -> l :: a | _ -> a)
      []
      x
    |> List.rev
  let rights x =
    foldl
      (fun a -> function Right r | Both (_, r) -> r :: a | _ -> a)
      []
      x
    |> List.rev

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

  ;; assert (to_list a = lefts c)
  ;; assert (to_list b = rights c)

  ;; ok "Align"
end

