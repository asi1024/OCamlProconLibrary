(* list *)
let rec print_list = function
    [] -> pf ""
  | [x] -> pf "%d\n" x
  | x :: xs -> pf "%d " x; print_list xs
;;

(* array *)
let intlist_of_intarray s len =
  let rec inner rv = function
      0 -> rv
    | m -> inner (Array.get s (m-1) :: rv) (m-1) in
  inner [] len
;;

let array_add ary pos v = Array.set ary pos (Array.get ary pos + v);;

(* string *)
let charlist_of_string s =
  let rec inner rv = function
      0 -> rv
    | m -> inner (s.[m-1]::rv) (m-1) in
  inner [] (String.length s)
;;
