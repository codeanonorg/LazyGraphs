type 'a lazy_tree =
    LNode of 'a * (( string option *'a lazy_tree Lazy.t) list)

type 'a tree = Node of 'a * (string option * 'a tree) list

let lnode a t = LNode (a, t)

let rec to_tree n (LNode (v, next)) =
  if n = 1 then Node (v, [])
  else Node (v, List.map (fun (o, q) -> o, to_tree (n - 1) (Lazy.force q)) next)

let no_dup l = List.fold_left (fun acc x -> if List.mem x acc then acc else x::acc) [] l

let rec edges (Node (v, next)) =
  List.fold_left (fun acc (o, Node (v', _)) -> (v, o, v')::acc)
    (next |> List.map (fun (_, t) -> edges t) |> List.concat) next |> no_dup

let dump pp f t =
  let out = open_out f in
  Printf.fprintf out "digraph {\n";
  List.iter (fun (x, o, y) ->
      match o with
      | None ->
        Printf.fprintf out "%s -> %s;\n" (pp x) (pp y)
      | Some l ->
        Printf.fprintf out "%s -> %s [ label = \"%s\"];\n" (pp x) (pp y) l
    ) (edges t);
  Printf.fprintf out "}\n"
