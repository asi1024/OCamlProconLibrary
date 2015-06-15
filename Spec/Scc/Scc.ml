let _ =
  let (v, e) = sf "%d %d\n" id2 in
  let g = make_graph v in
  for i = 1 to e do
    let (s, t) = sf "%d %d\n" id2 in
    add_edge g @@ make_edge2 s t
  done;
  let ary = scc g in
  let q = sf "%d\n" id in
  for i = 1 to q do
    let (u, v) = sf "%d %d\n" id2 in
    pf "%d\n" @@ if ary.(u) == ary.(v) then 1 else 0
  done
