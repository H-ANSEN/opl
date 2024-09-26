type t =
  | Var of identifier * int  (* variable name, de bruijn index *)
  | Lambda of identifier * t
  | Application of t * t

and identifier = string

module IdentifierSet = Set.Make ( String )

(** Parser converting string based representations of untyped strict lambda
    calculus into an ast of type [t]. Parser makes use of a context to collect
    de bruijn indices as input is being processed. This also has the advantage
    of identifying free and bound variables during parsing. *)
module Parser : sig
  val parse : Parsers.Input.t -> t Parsers.result
end = struct 
  open Parsers
  open Parsers.Let_syntax

  type context = {
    depth : int;
    b_map : identifier list;
    f_map : identifier list;
  }

  let find_ctx var ctx =
    let find = List.find_index (fun id -> id = var) in
    match find ctx.b_map with
    | Some i -> i, ctx
    | None ->
        match find ctx.f_map with
        | Some i -> i + ctx.depth, ctx
        | None ->
            let idx = ctx.depth + List.length ctx.f_map in
            let ctx = { ctx with f_map = List.append ctx.f_map [var] } in
            idx, ctx
  ;;

  let rec parse input =
    let initial_context = { depth = 0; b_map = []; f_map = [] } in
    (let+ (expression, _) = exp ~ctx:initial_context in
     expression)
    input

  and exp ~ctx input =
    (space *> (var ctx <|> lam ctx <|> app ctx)) input

  and var ctx input =
    (let* var = identifier in
     let index, ctx = find_ctx var ctx in 
     return (Var (var, index), ctx))
    input

  and app ctx input =
    (let* callee, ctx = charp '(' *> space *> exp ~ctx <* space in
     let* argument, ctx = exp ~ctx <* space <* charp ')' in
     return (Application (callee, argument), ctx))
    input

  and lam ctx input =
    (let* bound_var = charp '\\' *> identifier in
     let scope_bound_ctx = { ctx with depth = ctx.depth + 1;
                                      b_map = bound_var :: ctx.b_map; } in
     let* body, _ = charp '.' *> exp ~ctx:scope_bound_ctx <* space in
     return (Lambda (bound_var, body), ctx))
    input

  and identifier = map alpha ~f:Char.escaped
  and space = many (charp ' ' <|> charp '\t' <|> charp '\n' <|> charp '\r')
end

let rec to_string = function
  | Var (id, _) -> id
  | Lambda (id, body) -> Printf.sprintf "λ%s.%s" id (to_string body)
  | Application (e0, e1) -> Printf.sprintf "(%s %s)" (to_string e0) (to_string e1)
;;

let rec to_string_nameless = function
  | Var (_, idx) -> Int.to_string idx
  | Lambda (_, body) -> Printf.sprintf "λ %s" (to_string_nameless body)
  | Application (e0, e1) -> Printf.sprintf "(%s %s)" (to_string_nameless e0) (to_string_nameless e1)
;;

let of_string str =
  let input = Parsers.Input.make str in
  Result.map (fun (result, _) -> result) (Parser.parse input)
;;

let free_vars =
  let free_vars = IdentifierSet.empty in
  let rec collect_fvars set depth = function
    | Var (id, idx) when idx > depth -> IdentifierSet.add id set (* free var  *)
    | Var _ -> IdentifierSet.empty                               (* bound var *)
    | Lambda (_, body) -> collect_fvars set (depth + 1) body
    | Application (e0, e1) ->
        IdentifierSet.union (collect_fvars set depth e0) (collect_fvars set depth e1)
  in
  collect_fvars free_vars (-1) 
;;
