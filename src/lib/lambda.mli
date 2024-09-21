type t =
  | Var of identifier
  | Lambda of identifier * t
  | Application of t * t

and identifier = string

module IdentifierSet : Set.S with type elt = identifier

val to_string : t -> string
val of_string : string -> (t, Parsers.Error.t) Result.t

(** [free_vars e] returns the set of free or unbound variables in the expression
    [e]. Variables that fall outside of the scope of an abstraction are said to
    be free or unbound. *)
val free_vars : t -> IdentifierSet.t

(** [bound_vars e] returns the set of bound variables in the expression [e].
    Variables that fall within the scope of an abstraction are said to be
    bound. *)
val bound_vars : t -> IdentifierSet.t
