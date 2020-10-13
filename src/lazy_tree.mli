(**
   A module implementing infinite "trees" as a compact and efficient
   way to model infinite graphs (with finite arity).
*)

(** Type of lazy trees *)
type 'a t

type 'a tree

val lnode : 'a -> (string option * 'a t Lazy.t) list -> 'a t
(** Constructor for lazy nodes *)

(** Generates a concrete tree of depth [n] from a lazy tree *)
val to_tree : int -> 'a t -> 'a tree

(** Remove duplication from a list *)
val no_dup : 'a list -> 'a list

(** List edges of a tree *)
val edges : 'a tree -> ('a * string option * 'a) list 


(** Dump a tree in a file in dot format *)
val dump : ('a -> string) -> string -> 'a tree -> unit
