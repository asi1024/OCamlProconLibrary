type 'a skew_data =
  | Empty
  | Node of 'a * 'a skew_data * 'a skew_data

type 'a skew_heap =
  { insert : 'a -> unit; is_empty : unit -> bool; extract_min : unit -> 'a }

let new_skew_heap cmp =
  let heap = ref Empty in
  let singleton x = Node (x, Empty, Empty) in
  let rec union t1 t2 = match (t1, t2) with
      (Empty, _) -> t2
    | (_, Empty) -> t1
    | (Node (x1, l1, r1), Node (x2, l2, r2))
      -> if cmp x1 x2 < 0
         then Node (x1, (union t2 r1), l1)
         else Node (x2, (union t1 r2), l2) in
  let this () =
    { insert = (fun x -> heap := union (singleton x) !heap);
      is_empty = (fun () -> match !heap with Empty -> true | _ -> false);
      extract_min = (fun () -> match !heap with
                                 Empty -> raise (Error "Heap Empty")
                               | Node (x, l, r) -> heap := union l r; x) } in
  this ()
;;
