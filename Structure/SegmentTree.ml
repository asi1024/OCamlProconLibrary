(* Segment Tree *)
module SegmentTree =
  struct
    type 'a segment_tree = int * 'a array * 'a * ('a -> 'a -> 'a)
    let make n id f = ((1 lsl n, A.make (1 lsl (n + 1)) id, id, f) : 'a segment_tree)
    let update ((n, ar, id, f) : 'a segment_tree) p v =
      let rec upd p v =
        ar.(p) <- v;
        if p < 2*n-1 then
          let num = if p mod 2 == 0 then f ar.(p) ar.(p+1) else f ar.(p-1) ar.(p) in
          upd (p/2+n) num in
      upd p v
    let query ((n, ar, id, f) : 'a segment_tree) l r =
      let rec sub v la ra =
        if ra <= l || r <= la then id
        else if l <= la && ra <= r then ar.(v)
        else let nv, mid = (v - n) * 2, (la + ra) / 2 in
             f (sub nv la mid) (sub (nv+1) mid ra) in
      sub (2*n-2) 0 n
  end
;;

module ST = SegmentTree


(* Persistent Segment Tree *)
module SegmentTree =
  struct
    type 'a data =
      | Nil
      | Node of int * int * 'a * 'a data * 'a data
    type 'a segment_tree = 'a data * 'a * ('a -> 'a -> 'a)
    let make n id f =
      let rec sub l r =
        if r == l + 1
        then Node (l, r, id, Nil, Nil)
        else let m = (l + r) / 2 in Node (l, r, id, sub l m, sub m r) in
      ((sub 0 (1 lsl n), id, f) : 'a segment_tree)
    let value_of = function Nil -> err "val" | Node (_, _, v, _, _) -> v
    let update ((node, id, f) : 'a segment_tree) p v =
      let rec upd node p v =
        match node with
          Nil -> err "upd"
        | Node (l, r, _, ls, rs) ->
           if ls == Nil then Node (l, r, v, ls, rs)
           else let m = (l + r) / 2 in
                let ls, rs = if p < m then upd ls p v, rs else ls, upd rs p v in
                Node (l, r, f (value_of ls) (value_of rs), ls, rs) in
      ((upd node p v, id, f) : 'a segment_tree)
    let query ((node, id, f) : 'a segment_tree) lpos rpos =
      let rec sub node =
        match node with
          Nil -> err "query"
        | Node (l, r, v, ls, rs) ->
           if r <= lpos || rpos <= l then id
           else if lpos <= l && r <= rpos then v
           else f (sub ls) (sub rs) in
      sub node
  end
;;

module ST = SegmentTree
