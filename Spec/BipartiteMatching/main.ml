let _ =
  let (x, y, e) = sf "%d %d %d\n" id3 in
  let g = make_matching (x + y) in
  for i = 1 to e do
    let (s, t) = sf "%d %d\n" id2 in
    add_match g s (x + t)
  done;
  let (res, _) = bipartite_matching g in
  pf "%d\n" res
