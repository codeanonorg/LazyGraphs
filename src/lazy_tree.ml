type 'a lazy_tree =
    LNode of 'a * (('a lazy_tree Lazy.t) list)

type 'a tree = Node of 'a * 'a tree list

let lnode a t = LNode (a, t)

let rec to_tree n (LNode (v, next)) =
  if n = 1 then Node (v, [])
  else Node (v, List.map (fun q -> to_tree (n - 1) (Lazy.force q)) next)


let dump pp f t =
  let out = open_out f in
  let rec step (Node (v, s)) =
    List.iter (fun (Node (v', _) as s') ->
        Printf.fprintf out "%s -> %s;\n" (pp v) (pp v');
        step s'
      ) s;
  in
  Printf.fprintf out "digraph {\n";
  step t;
  Printf.fprintf out "}\n"
