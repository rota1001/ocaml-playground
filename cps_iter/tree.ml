type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec inorder_iter (t: 'a tree) (f: 'a -> (unit -> 'b) -> 'b) (k: unit -> 'b) : 'b =
  match t with
    | Leaf -> k ()
    | Node(l, x, r) -> inorder_iter l f (fun () -> f x (fun () -> inorder_iter r f k))


exception StopIteration

let imperative_gen (t: 'a tree) : ('a -> unit) -> unit =
    let rec next = ref (
      fun k -> raise StopIteration
    ) in
    next := (fun (k: 'a -> unit) ->
      k (inorder_iter t (fun x k1 -> next := (fun k2 -> k2 (k1 ())); x) (fun () -> raise StopIteration)));
    fun k -> !next k

type 'a enum = None | More of 'a * (unit -> 'a enum)

let pure_enum (t: 'a tree) : 'a enum = 
  inorder_iter t (fun x k -> More (x, k)) (fun () -> None)

let () =
  let root = Node(
    Node(Leaf, 2, Leaf),
    1,
    Node(Leaf, 3,
      Node(Leaf, 4, Leaf)
    )
  ) in
  inorder_iter root (fun (x: 'a) (k: unit -> unit) : unit -> Printf.printf "%d\n" x; k ()) (fun () -> ());
  let g = imperative_gen root in
  g (fun x -> Printf.printf "x1: %d\n" x);
  g (fun x -> Printf.printf "x2: %d\n" x);
  g (fun x -> Printf.printf "x3: %d\n" x);
  g (fun x -> Printf.printf "x4: %d\n" x);
  let e = pure_enum root in
  let rec f (e: int enum) =
    match e with
      | None -> ()
      | More(x, k) ->
        Printf.printf "%d\n" x;
        f (k ())
  in
  f e

