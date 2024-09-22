type t =
  | Var of identifier * int  (* variable name, de bruijn index *)
  | Lambda of identifier * t
  | Application of t * t

and identifier = string

(** Parser converting string based representations of lambda calculus into an
    ast of type [t]. Parser makes use of a context to collect de bruijn indices
    as input is being processed. This also has the advantage of identifying
    free and bound variables during parsing.

    Currently any free variable's de burijn index is set to [-1]. Possible todo
    is include a set of free or bound variables in our type representation so we
    dont have to walk the AST again! *)
module Parser : sig
  val parse : Parsers.Input.t -> t Parsers.result
end = struct 
  open Parsers
  open Parsers.Let_syntax

  let find_ctx (c : identifier) (ctx : string list) : int =
    Option.value ~default:(-1) @@
      List.find_index (fun s -> c = s) ctx
  ;;

  let rec parse input =
    (let+ (expr, _) = exp ~ctx:[] in (* dispose of context *)
     expr)
    input

  and exp ~(ctx : string list) (input : Input.t) =
    (let* expr, ctx = space *> (var ctx <|> lambda ctx <|> application ctx) in
     return (expr, ctx))
    input

  and var ctx input =
    (let* var = identifier in
     let index = find_ctx var ctx in
     return (Var (var, index), ctx))
    input

  and application ctx input =
    (let* callee, ctx = charp '(' *> space *> exp ~ctx <* space in
     let* argument, ctx = exp ~ctx <* space <* charp ')' in
     return (Application (callee, argument), ctx))
    input

  and lambda ctx input =
    (let* var = charp '\\' *> identifier in
     let scope_bound_ctx = var :: ctx in
     let* body, _ = charp '.' *> exp ~ctx:scope_bound_ctx <* space in
     return (Lambda (var, body), ctx))
    input

  and identifier = map alpha ~f:Char.escaped
  and space = many (charp ' ' <|> charp '\t' <|> charp '\n' <|> charp '\r')
end

let rec to_string ?(indices=false) = function
  | Var (_, x) when indices -> (Int.to_string x)
  | Var (id, _) -> id
  | Lambda (_, body) when indices -> Printf.sprintf "λ %s" (to_string ~indices:true body)
  | Lambda (id, body) -> Printf.sprintf "λ%s.%s" id (to_string body)
  | Application (e0, e1) -> Printf.sprintf "(%s %s)" (to_string ~indices e0) (to_string ~indices e1)
;;

let of_string str =
  let input = Parsers.Input.make str in
  Result.map (fun (result, _) -> result) (Parser.parse input)
;;
