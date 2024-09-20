type t =
  | Var of identifier
  | Lambda of identifier * t
  | Application of t * t

and identifier = string

module IdentifierSet : Set.S with type elt = identifier

val to_string : t -> string
val of_string : string -> (t, Parsers.Error.t) Result.t

(** [free_vars e] returns the set of free or unbound variabels in the expression
    [e]. Variables that fall within the scope of an abstraction are said to be
    bound. *)
val free_vars : t -> IdentifierSet.t
