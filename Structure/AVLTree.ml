(* map *)
type ('a, 'b) avl_data =
  | Empty
  | Node of int * ('a * 'b) * ('a, 'b) avl_data * ('a, 'b) avl_data
;;

type 'a option = Nothing | Just of 'a;;

type ('a, 'b) avl_map =
  { set : ('a * 'b) -> unit; value : 'a -> 'b option }
;;

let new_avl_map cmp =
  let tree = ref Empty in
  let err() = raise @@ Error "AVL-Tree Internal Error." in
  let height = function Empty -> 0 | Node (h, _, _, _) -> h in
  let make_node a l r = Node (max (height l) (height r) + 1, a, l, r) in
  let rotate = function
      Empty -> Empty
    | Node (_, a, l, r) ->
       if height l > height r + 1
       then (match l with
               Empty -> err()
             | Node (_, b, ll, lr) ->
                if height ll >= height lr
                then make_node b ll (make_node a lr r)
                else (match lr with
                        Empty -> err()
                      | Node (_, c, lrl, lrr) ->
                         make_node c (make_node b ll lrl) (make_node a lrr r)))
       else if height l + 1 < height r
       then (match r with
               Empty -> err()
             | Node (_, b, rl, rr) ->
                if height rl <= height rr
                then make_node b (make_node a l rl) rr
                else (match rl with
                        Empty -> err()
                      | Node (_, c, rll, rlr) ->
                         make_node c (make_node a l rll) (make_node b rlr rr)))
       else make_node a l r in
  let rec insert (x, y) = function
      Empty -> Node (1, (x, y), Empty, Empty)
    | Node (h, (k, v), l, r) ->
       let t = if cmp x k = 0
               then Node (h, (x, y), l, r)
               else if cmp x k < 0
               then Node (h, (k, v), insert (x, y) l, r)
               else Node (h, (k, v), l, insert (x, y) r) in
       rotate t in
  let rec find x = function
      Empty -> Nothing
    | Node (_, (k, v), l, r) ->
       if cmp x k = 0
       then Just v
       else if cmp x k < 0
       then find x l
       else find x r in
  let this() =
    { set = (fun x -> tree := insert x !tree);
      value = (fun x -> find x !tree) } in
  this();
;;
