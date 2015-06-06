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

let err s = raise (Failure s)

let inf = 1000000000
let eps = 1e-11

let _ =
  let (n, m) = sf "%d %d\n" (fun x y -> x, y) in
  let p = PQ.init in
  pf "%d\n" @@ n + m;
  pf "%s\n" @@ string_of_list string_of_int @@ PQ.list_of p
;;
