(* Graph *)
type 'a edge_t = (int * int * 'a)
type 'a graph_t = 'a edge_t list array

let make_edge3 src dest cost = ((src, dest, cost) : 'a edge_t)

let src  ((s, _, _) : 'a edge_t) = s
let dest ((_, d, _) : 'a edge_t) = d
let cost ((_, _, c) : 'a edge_t) = c

let make_graph v = (A.make v [] : 'a graph_t)

let add_edge (g : 'a graph_t) (e : 'a edge_t) =
  let s = src e in A.set g s (e :: A.get g s);;
