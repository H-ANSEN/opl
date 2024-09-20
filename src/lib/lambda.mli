type t =
  | Var of identifier
  | Lambda of identifier * t
  | Application of t * t

and identifier = string

val to_string : t -> string
val of_string : string -> (t, Parsers.Error.t) Result.t
