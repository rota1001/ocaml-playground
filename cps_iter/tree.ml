type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec inorder_iter (t: 'a tree) (f: 'a -> (unit -> 'b) -> 'b) (k: unit -> 'b) : 'b =
  match t with
    | Leaf -> k ()
    | Node(l, x, r) -> inorder_iter l f (fun () -> f x (fun () -> inorder_iter r f k))

let () =
  let root = Node(
    Node(Leaf, 2, Leaf),
    1,
    Node(Leaf, 3,
      Node(Leaf, 4, Leaf)
    )
  ) in
  inorder_iter root (fun (x: 'a) (k: unit -> unit) : unit -> Printf.printf "%d\n" x; k ()) (fun () -> ())
