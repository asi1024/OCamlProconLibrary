(* Priority Queue *)
module Priority_Queue =
  struct
    type 'a data =
      | Empty
      | Node of 'a * 'a data * 'a data
    let init = Empty
    let singleton x = Node (x, Empty, Empty)
    let rec union t1 t2 = match (t1, t2) with
        (Empty, _) -> t2
      | (_, Empty) -> t1
      | (Node (x1, l1, r1), Node (x2, l2, r2))
        -> if x1 < x2
           then Node (x1, (union t2 r1), l1)
           else Node (x2, (union t1 r2), l2)
    let insert x h = union (singleton x) h
    let is_empty h = (h == Empty)
    let extract_min = function
        Empty -> err "Heap Empty"
      | Node (x, l, r) -> (x, union l r)
    let rec list_of = function
        Empty -> []
      | Node (x, l, r) -> x :: list_of l @ list_of r
  end
;;

module PQ = Priority_Queue
