let output_int n = if n == inf then pf "INF\n" else pf "%d\n" n

let _ =
  let (v, e, r) = sf "%d %d %d\n" id3 in
  let g = make_graph v in
  for i = 1 to e do
    let (s, t, d) = sf "%d %d %d\n" id3 in
    add_edge g @@ make_edge3 s t d
  done;
  let (b, ary) = spfa g r 0 inf in
  if b then A.iter output_int ary else pf "NEGATIVE CYCLE\n"
