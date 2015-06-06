(* Disjoint Set *)
module UnionFind =
  struct
    type unionfind = int array
    let make n = (A.make n (-1) : unionfind)
    let rec root x (uf : unionfind) =
      let p = uf.(x) in
      if p < 0 then x else let r = root p uf in uf.(x) <- r; r
    let merge x y uf =
      let x, y = root x uf, root y uf in
      if x != y then
        let (x, y) = if uf.(y) < uf.(x) then (y, x) else (x, y) in
        if uf.(x) == uf.(y) then uf.(x) <- uf.(x) - 1;
        uf.(y) <- x;
  end
;;

module UF = UnionFind
