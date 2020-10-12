open Lazy_tree

(*
  cas1 : a>=3 => a-=3; v8 += 1;
  cas2 : a>=2 => a-=2; v4 += 1;
  cas3 : b>=2 => b-=2; v9 += 1;
  cas4 : a>= 1 & b >= 1 => a -= 1; b-= 1; v6 += 1;
*)

type state = {
  a   : int;
  b   : int;
  v4  : int;
  v6  : int;
  v8  : int;
  v9  : int;
}

type rule = {
  pred_ab : (int * int) -> bool;
  transition : state -> state;
}

let mk_rule p t = {pred_ab = p; transition = t}

let rule_1 =
  let p (a, _) = a >= 3 in
  let t st =
    {st with a = st.a - 3; v8 = st.v8 + 1}
  in mk_rule p t

let rule_2 =
  let p (a, _) = a >= 2 in
  let t st =
    {st with a = st.a - 2; v4 = st.v4 + 1}
  in mk_rule p t

let rule_3 =
  let p (_, b) = b >= 2 in
  let t st =
    {st with b = st.b - 2; v9 = st.v9 + 1}
  in mk_rule p t

let rule_4 =
  let p (a, b) = a >= 1 && b >= 1 in
  let t st =
    {st with a = st.a - 1; b = st.b - 1; v6 = st.v6 + 1}
  in mk_rule p t

let rules = [rule_1; rule_2; rule_3; rule_4]

let check q rl = rl.pred_ab q

let trans st rl = rl.transition st

let no_dup l = List.fold_left (fun acc x -> if List.mem x acc then (print_endline "OK"; acc) else x::acc) [] l

let rec generate st0 =
  List.filter (check (st0.a, st0.b)) rules
  |> List.map (trans st0)
  |> no_dup
  |> List.map (fun s -> lazy (generate s))
  |> lnode st0

let pp ({a; b; v4; v6; v8; v9}) = Printf.sprintf "\"{%d, %d, %d, %d, %d, %d}\"" a b v4 v6 v8 v9

let start = {a = 19; b = 4; v4 = 0; v6 = 0; v8 = 0; v9 = 0}

let _ = to_tree 4 (generate start)
        |> dump pp "test.dot"
