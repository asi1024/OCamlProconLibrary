(* Graph *)
type 'a edge_t = (int * int)
type 'a graph_t = 'a edge_t list array

let make_edge2 src dest = ((src, dest) : 'a edge_t)

let src  ((s, _) : 'a edge_t) = s
let dest ((_, d) : 'a edge_t) = d

let make_graph v = (A.make v [] : 'a graph_t)

let add_edge (g : 'a graph_t) (e : 'a edge_t) =
  let s = src e in A.set g s (e :: A.get g s)

let scc g =
  let n = A.length g in let h = make_graph n in
  let used, cmp = A.make n false, A.make n 0 in
  let rec dfs l v =
    if used.(v) then l else (used.(v) <- true; v :: (L.fold_left dfs l @@ L.map dest g.(v))) in
  let rec rdfs k v =
    if used.(v) then (used.(v) <- false; cmp.(v) <- k; L.iter (rdfs k) @@ L.map dest h.(v)) in
  let vs = A.fold_left dfs [] @@ A.init n (fun x -> n - x - 1) in
  A.iter (L.iter (fun e -> add_edge h @@ make_edge2 (dest e) (src e))) g;
  L.fold_left (fun x v -> if used.(v) then (rdfs x v; x + 1) else x) 0 vs |> ignore;
  cmp
;;
