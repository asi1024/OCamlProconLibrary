let _ =
  let (n, q) = sf "%d %d\n" id2 in
  let uf = UF.make n in
  for i = 1 to q do
    let (com, x, y) = sf "%d %d %d\n" id3 in
    if com == 0
    then UF.merge x y uf
    else pf "%d\n" (if UF.root x uf == UF.root y uf then 1 else 0)
  done
