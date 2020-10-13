open Lazy_tree

(**
   Modules describing a state
*)
module type STATE = sig
  type t
  val to_string : t -> string
end

(**
   Modules describing a transition system over states S
*)
module Make (S : STATE) : sig
  (** Rules of the transition system *)
  type rule

  val mk_rule : string option -> (S.t -> bool) -> (S.t -> S.t) -> rule
  (** Instanciate a rule *)

  (** [generate start rules] generates
      a lazy_tree representing all states reachable from state start
      with respect to the transitions described in [rules] *)
  val generate : S.t -> rule list -> S.t lazy_tree

  (** Check if a rule is compatible with a given state *)
  val satisfy : S.t -> rule -> bool

  (** Apply a rule to a state. Returns the (optional) name of 
      the rule together with the new state *)
  val produce : S.t -> rule -> (string option * S.t)

end = struct
  type rule = {
    name : string option;
    pred : S.t -> bool;
    trans : S.t -> S.t;
  }

  let mk_rule name pred trans = {name; pred; trans}

  let satisfy q rl = rl.pred q

  let produce st rl = (rl.name, rl.trans st)

  let rec generate st0 rls =
    List.filter (satisfy st0) rls
    |> List.map (produce st0)
    |> no_dup
    |> List.map (fun (rl, s) -> rl, lazy (generate s rls))
    |> lnode st0
end

(* open Lazy_tree *)

(* (*
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
   name : string;
   pred_ab : (int * int) -> bool;
   transition : state -> state;
   }

   let mk_rule s p t = {name = s; pred_ab = p; transition = t}

   let rule_1 =
   let p (a, _) = a >= 3 in
   let t st =
    {st with a = st.a - 3; v8 = st.v8 + 1}
   in mk_rule "rule_1" p t

   let rule_2 =
   let p (a, _) = a >= 2 in
   let t st =
    {st with a = st.a - 2; v4 = st.v4 + 1}
   in mk_rule "rule_2" p t

   let rule_3 =
   let p (_, b) = b >= 2 in
   let t st =
    {st with b = st.b - 2; v9 = st.v9 + 1}
   in mk_rule "rule_3" p t

   let rule_4 =
   let p (a, b) = a >= 1 && b >= 1 in
   let t st =
    {st with a = st.a - 1; b = st.b - 1; v6 = st.v6 + 1}
   in mk_rule "rule_4" p t

   let rules = [rule_1; rule_2; rule_3; rule_4]

   let check q rl = rl.pred_ab q

   let trans st rl = (Some rl.name, rl.transition st)

   let rec generate st0 =
   List.filter (check (st0.a, st0.b)) rules
   |> List.map (trans st0)
   |> List.map (fun (rl, s) -> rl, lazy (generate s))
   |> lnode st0

   let pp ({a; b; v4; v6; v8; v9}) = Printf.sprintf "\"%d %d %d %d %d %d\"" a b v4 v6 v8 v9

   let start = {a = 19; b = 4; v4 = 0; v6 = 0; v8 = 0; v9 = 0}

   let _ = to_tree 20 (generate start)
        |> dump pp "test.dot" *)
