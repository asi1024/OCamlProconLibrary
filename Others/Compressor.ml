let unique l =
  let rec sub res l =
    match res, l with
      (_, []) -> res
    | ([], x :: xs) -> sub [x] xs
    | (y :: ys, x :: xs) -> sub (if x == y then res else x :: res) xs in
  sub [] @@ L.rev l

let compressor ar =
  let w = A.copy ar in A.sort compare w;
  A.map (lower_bound @@ A.of_list @@ unique @@ A.to_list w) ar
