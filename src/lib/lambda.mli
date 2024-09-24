type t
type identifier = string

module IdentifierSet : Set.S with type elt = identifier

val of_string : string -> (t, Parsers.Error.t) Result.t
val to_string : ?indices:bool -> t -> string

(** [free_vars e] returns the set of free or unbound variables in the expression
    [e]. Variables that fall outside the scope of an abstraction are said to be
    free/unbound *)
val free_vars : t -> IdentifierSet.t
