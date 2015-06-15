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

let spfa g s zero inf =
  let n, que = A.length g, Q.create() in
  let d, updated, inque = A.make n inf, A.make n (-1), A.make n false in
  let rec sub() =
    if Q.is_empty que then (true, d) else
      let v = Q.pop que in
      let func e =
        let w, c = dest e, d.(v) + cost e in
        if c < d.(w) then (d.(w) <- c; if not (inque.(w)) then Q.add w que; inque.(w) <- true) in
      inque.(v) <- false; updated.(v) <- updated.(v) + 1;
      if updated.(v) == n then (false, d) else (L.iter func g.(v); sub()) in
  d.(s) <- zero; Q.push s que; sub()
;;
