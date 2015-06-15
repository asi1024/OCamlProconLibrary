(* Graph *)
type 'a matching_t = int list array

let make_matching v = (A.make v [] : 'a matching_t)

let add_match (g : 'a matching_t) s t =
  g.(s) <- (t :: g.(s)); g.(t) <- (s :: g.(t))

let bipartite_matching g =
  let n = A.length g in let m = A.make n (-1) in
  let rec dfs v f =
    let sub u = (m.(u)<0 || (f.(m.(u))&&dfs m.(u) f)) && (m.(v)<-u; m.(u)<-v; true) in
    f.(v) <- false; L.fold_left (fun x y -> x || sub y) false g.(v) in
  let func r v = if m.(v) < 0 && dfs v (A.make n true) then r+1 else r in
  (A.fold_left func 0 (A.init n id), m)
;;
