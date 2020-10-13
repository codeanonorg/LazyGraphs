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

  val mk_rule : ?name:string -> (S.t -> bool) -> (S.t -> S.t) -> rule
  (** Instanciate a rule *)

  (** [generate start rules] generates
      a lazy_tree representing all states reachable from state start
      with respect to the transitions described in [rules] *)
  val generate : S.t -> rule list -> S.t t

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

  let mk_rule ?name pred trans = {name = name; pred; trans}

  let satisfy q rl = rl.pred q

  let produce st rl = (rl.name, rl.trans st)

  let rec generate st0 rls =
    List.filter (satisfy st0) rls
    |> List.map (produce st0)
    |> no_dup
    |> List.map (fun (rl, s) -> rl, lazy (generate s rls))
    |> lnode st0
end