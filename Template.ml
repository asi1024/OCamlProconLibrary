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

let _ =
  let (n, m) = sf "%d %d\n" id2 in
  pf "%d\n" @@ n + m
;;
