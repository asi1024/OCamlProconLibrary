(* Graph *)
type 'a edge_t = (int * int * 'a)
type 'a graph_t = 'a edge_t list array

let make_edge3 src dest cost = ((src, dest, cost) : 'a edge_t)

let src  ((s, _, _) : 'a edge_t) = s
let dest ((_, d, _) : 'a edge_t) = d
let cost ((_, _, c) : 'a edge_t) = c

let make_graph v = (A.make v [] : 'a graph_t)

let add_edge (g : 'a graph_t) (e : 'a edge_t) =
  let s = src e in A.set g s (e :: A.get g s)

let dijkstra g s zero inf =
  let n = A.length g in
  let d = A.make n inf in
  let rec sub que =
    if not (PQ.is_empty que) then
      let ((x, y), q) = PQ.extract_min que in
      let upd q e =
        let v, c = dest e, x + cost e in
        if d.(v) > c then (d.(v) <- c; PQ.insert (d.(v), v) q) else q in
      if d.(y) >= x then sub @@ L.fold_left upd q g.(y) else sub q in
  d.(s) <- zero; sub (PQ.singleton (d.(s), s)); d
;;
