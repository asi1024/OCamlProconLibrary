module A = Array
module C = Char
module I = Int64
module L = List
module Q = Queue
module S = String

let pf = Printf.printf
let sf = Scanf.scanf
let ssf = Scanf.sscanf

let (|>) x f = f x
let (@@) f x = f x

let id x = x
let id2 x y = (x, y)
let id3 x y z = (x, y, z)
let id4 a b c d = (a, b, c, d)
let id5 a b c d e = (a, b, c, d, e)

let err s = raise (Failure s)

let inf = 1000000000
let eps = 1e-11

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

let _ =
  let (n, q) = sf "%d %d\n" id2 in
  let uf = UF.make n in
  for i = 1 to q do
    let (com, x, y) = sf "%d %d %d\n" id3 in
    if com == 0
    then UF.merge x y uf
    else pf "%d\n" (if UF.root x uf == UF.root y uf then 1 else 0)
  done
