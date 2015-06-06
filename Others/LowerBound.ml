(* lower bound *)
let lower_bound ar v =
  let rec sub l r =
    if r - l <= 1 then r
    else let mid = (l + r) / 2 in
         if (ar.(mid) >= v) then sub l mid else sub mid r in
  sub (-1) @@ A.length ar
