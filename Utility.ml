(* list *)
let string_of_list f l =
  let rec func = function
      [] -> "]"
    | (x :: xs) -> ", " ^ f x ^ func xs in
  match l with
    [] -> "[]"
  | (x :: xs) -> "[" ^ f x ^ func xs
;;

(* string *)
let charlist_of_string s =
  let rec inner rv = function
      0 -> rv
    | m -> inner (s.[m-1]::rv) (m-1) in
  inner [] (String.length s)
;;
