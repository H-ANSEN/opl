type t =
  | Var of identifier * int
  | Lambda of identifier * t
  | Application of t * t

and identifier = string

val to_string : ?indices:bool -> t -> string
val of_string : string -> (t, Parsers.Error.t) Result.t
