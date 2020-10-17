(** A labeled graphs *)
type 'a graph = ('a option * int) list array

(**
   Create a new (directed) graph with [n] vertices
   and no edges
*)
let create_graph n = Array.init n (fun _ -> [])

let vertices g = Array.length g

let edges g = Array.fold_left (fun acc v -> acc + List.length v) g

(**
   Remove the edge [(a, b)] from a graph [g] if it exists.
*)
let rem_edge g a b =
  if 0 <= a && a <= vertices g then
    g.(a) <- List.filter (fun (_, x) -> x <> b) g.(a)
  else failwith "rem_edge : invalid vertex"

(**
   Add the edge [(a, b)] in a graph [g].
   The edge may be labeled.

   It the edge already exists, it is replaced.
*)
let add_edge g a ?label b =
  rem_edge g a b;
  g.(a) <- (label, b)::g.(a)