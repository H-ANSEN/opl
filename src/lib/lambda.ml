type t =
  | FreeVar of identifier
  | BoundVar of identifier * int  (* variable name, de bruijn index *)
  | Lambda of identifier * t
  | Application of t * t

and identifier = string

module IdentifierSet = Set.Make ( String )

(** Preprocessor converting extended lambda calculus into a more strict
    representation able to be processed by the parser. This creates a sepration
    allowing the parser to only worry about parsing the stricter subset while
    the preprocessor worries about the following extensions.

    - Outermost parenthses are added: M N becomes (M N)
    - Applications are rewritten to be left associative: M N P becomes (M (N P))
    - Sequences of abstractions are expanded: λxyz.x becomes \x.\y.\z.x
    - The body of abstractions is extended as far right as possible *)
module Preprocess = struct

  (** TO-DO *)

end

(** Parser converting string based representations of untyped strict lambda
    calculus into an ast of type [t]. Parser makes use of a context to collect
    de bruijn indices as input is being processed. This also has the advantage
    of identifying free and bound variables during parsing. *)
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
     let value = if index = -1 then FreeVar var else BoundVar (var, index) in
     return (value, ctx))
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
  | FreeVar id -> id
  | BoundVar (id, idx) -> if indices then Int.to_string idx else id
  | Application (e0, e1) ->
    Printf.sprintf "(%s %s)" (to_string ~indices e0) (to_string ~indices e1)
  | Lambda (id, body) ->
    if indices then Printf.sprintf "λ %s" (to_string ~indices body)
    else Printf.sprintf "λ%s.%s" id (to_string body)
  ;;

let of_string str =
  let input = Parsers.Input.make str in
  Result.map (fun (result, _) -> result) (Parser.parse input)
;;

let free_vars =
  let set = IdentifierSet.empty in
  let rec collect_fvars set = function
    | BoundVar _ -> IdentifierSet.empty
    | FreeVar id -> IdentifierSet.add id set
    | Lambda (_, body) -> collect_fvars set body
    | Application (e0, e1) -> 
        IdentifierSet.union (collect_fvars set e0) (collect_fvars set e1)
  in
  collect_fvars set
;;

let rec subst ~name ~with' ~in' =
  match in' with
  | BoundVar _ -> in'
  | FreeVar id -> if name = id then with' else in'
  | Lambda (id, body) -> Lambda (id, subst ~name ~with' ~in':body)
  | Application (t1, t2) ->
      Application (subst ~name ~with' ~in':t1, subst ~name ~with' ~in':t2)
;;

let rec opening ~term ~depth ~with' =
  match term with
  | FreeVar _ -> term
  | BoundVar (_, k) -> if k = depth then with' else term
  | Lambda (id, body) -> Lambda (id, opening ~depth:(depth + 1) ~with' ~term:body)
  | Application (t1, t2) ->
      Application (opening ~depth ~with' ~term:t1, opening ~depth ~with' ~term:t2)
;;
