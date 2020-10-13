open Lazy_graph.Lazy_tree
open Lazy_graph.Transitions

(**
   We modelize a (potentialy) infinite system of transitions
   using lazy trees.

   State :
    (a, b, v4, v6, v8, v9) are 6 integers

   Transitions :
    rule_1 : a>=3 => a-=3; v8 += 1;
    rule_2 : a>=2 => a-=2; v4 += 1;
    rule_3 : b>=2 => b-=2; v9 += 1;
    rule_4 : a>= 1 & b >= 1 => a -= 1; b-= 1; v6 += 1;
*)

type state = {
  a   : int;
  b   : int;
  v4  : int;
  v6  : int;
  v8  : int;
  v9  : int;
}

let to_string ({a; b; v4; v6; v8; v9}) =
  Printf.sprintf "\"%d %d %d %d %d %d\"" a b v4 v6 v8 v9


module Trans = Make(struct
    type t = state
    let to_string = to_string
  end)


let rule_1 =
  let p st = st.a >= 3 in
  let t st =
    {st with a = st.a - 3; v8 = st.v8 + 1}
  in Trans.mk_rule ~name:"rule_1" p t

let rule_2 =
  let p st = st.a >= 2 in
  let t st =
    {st with a = st.a - 2; v4 = st.v4 + 1}
  in Trans.mk_rule ~name:"rule_2" p t

let rule_3 =
  let p st = st.b >= 2 in
  let t st =
    {st with b = st.b - 2; v9 = st.v9 + 1}
  in Trans.mk_rule ~name:"rule_3" p t

let rule_4 =
  let p st = st.a >= 1 && st.b >= 1 in
  let t st =
    {st with a = st.a - 1; b = st.b - 1; v6 = st.v6 + 1}
  in Trans.mk_rule ~name:"rule_4" p t

let rules = [rule_1; rule_2; rule_3; rule_4]


let start = {a = 19; b = 4; v4 = 0; v6 = 0; v8 = 0; v9 = 0}

(** EP *)
let _ = to_tree 8 (Trans.generate start rules)
        |> dump to_string "test.dot"