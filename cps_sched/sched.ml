let q: (unit -> unit) Queue.t = Queue.create ()

let sched () : unit =
  match Queue.take_opt q with
    | Some k -> k ()
    | None -> ()

let yield (k: unit -> unit) : unit =
  Queue.add k q;
  sched ()

let exit () : unit =
  sched ()

let spawn (k: unit -> unit) : unit =
  Queue.add k q

let process (name: string) (cnt: int) =
  let rec proc x =
    if x > cnt then exit () else begin
      Printf.printf "%s%d " name x;
      yield (fun () -> proc (x + 1))
    end
  in
  proc 1

let () =
  spawn (fun () -> process "A" 3);
  spawn (fun () -> process "B" 2);
  process "C" 5
