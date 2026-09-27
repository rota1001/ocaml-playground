let rec fact (n: int) (k: int -> unit) : unit =
  if n == 0 then k 1 else fact (n - 1) (fun (x: int): unit -> k (n * x))

let () =
  Printf.printf "helloworld\n";
  fact 5 (fun x -> Printf.printf "5! = %d\n" x)
