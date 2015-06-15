let inf = (1 lsl 31) - 1

let _ =
  let (n, q) = sf "%d %d\n" id2 in
  let seg = ref @@ ST.make 17 inf min in
  for i = 1 to q do
    let (com, x, y) = sf "%d %d %d\n" id3 in
    if com == 0
    then seg := ST.update !seg x y
    else pf "%d\n" @@ ST.query !seg x (y + 1)
  done
