type 'a vector =
  { init : 'a array -> unit;
    at : int -> 'a;
    set : int -> 'a -> unit;
    last_set : 'a -> unit;
    push_back : 'a -> unit;
    pop_back : unit -> unit;
    front : unit -> 'a;
    back : unit -> 'a;
    size : unit -> int; }
;;

let new_vector v =
  let ary = ref (Array.copy v) in
  let sz = ref (Array.length v) in
  let reserve a =
    if !sz > Array.length !ary
    then ( let nary = Array.make (Array.length !ary * 2) a in
           for i = 0 to Array.length !ary do
             Array.set nary i (Array.get !ary i)
           done;
           ary := nary ) in
  let rec this () =
    { init = (fun w -> ary := w; sz := Array.length w);
      at = (fun x -> Array.get !ary x );
      set = (fun x y -> Array.set !ary x y);
      last_set = (fun x -> (this()).set (!sz - 1) x);
      push_back = (fun x -> sz := !sz+1; reserve x; (this()).last_set x);
      pop_back = (fun () -> sz := !sz - 1);
      front = (fun () -> (this()).at 0);
      back = (fun () -> (this()).at (!sz - 1));
      size = (fun () -> !sz)} in
  this ()
;;
