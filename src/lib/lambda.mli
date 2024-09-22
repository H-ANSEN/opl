type t
type identifier = string

module IdentifierSet : Set.S with type elt = identifier

val to_string : ?indices:bool -> t -> string
val of_string : string -> (t, Parsers.Error.t) Result.t

(** [free_vars e] returns the set of free or unbound variables in the expression
    [e]. Variables that fall outside the scope of an abstraction are said to be
    free/unbound *)
val free_vars : t -> IdentifierSet.t

(** [subst name with' in'] substitutes all instances of the variable [name] with
    the lambda expression [with'] in the lambda expression [in']. *)
val subst : name:identifier -> with':t -> in':t -> t
