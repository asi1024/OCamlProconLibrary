let pf = Printf.printf;;
let sf = Scanf.scanf;;

let (|>) x f = f x;;
let (@@) f x = f x;;

exception Error of string

let inf = 1000000000000000000;;
let eps = 1e-11;;

let _ =
  let (n, m) = sf "%d %d\n" (fun x y -> x, y) in
  pf "%d\n" @@ n + m
;;
