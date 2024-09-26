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

  type ctx = {
    bvars : string list;
    fvars : (string * int) list;
  }

  let find_ctx id ctx =
    match List.find_index (fun s -> id = s) ctx.bvars with
    | Some idx -> idx, ctx
    | None ->
        match List.assoc_opt id ctx.fvars with
        | Some idx -> idx, ctx
        | None ->
            let idx = List.length ctx.fvars + List.length ctx.bvars in
            let ctx = { ctx with fvars = (id, idx) :: ctx.fvars } in
            idx, ctx
  ;;

  let rec parse input =
    (let+ (expr, _) = exp ~ctx:{ bvars=[]; fvars=[] } in (* dispose of context *)
     expr)
    input

  and exp ~ctx input =
    (let* expr, ctx = space *> (var ctx <|> lambda ctx <|> application ctx) in
     return (expr, ctx))
    input

  and var ctx input =
    (let* var = identifier in
     let index, ctx = find_ctx var ctx in
     return (Var (var, index), ctx))
    input

  and application ctx input =
    (let* callee, ctx = charp '(' *> space *> exp ~ctx <* space in
     let* argument, ctx = exp ~ctx <* space <* charp ')' in
     return (Application (callee, argument), ctx))
    input

  and lambda ctx input =
    (let* var = charp '\\' *> identifier in
     let scope_bound_ctx = { ctx with bvars = var :: ctx.bvars } in
     let* body, _ = charp '.' *> exp ~ctx:scope_bound_ctx <* space in
     return (Lambda (var, body), ctx))
    input

  and identifier = map alpha ~f:Char.escaped
  and space = many (charp ' ' <|> charp '\t' <|> charp '\n' <|> charp '\r')
end

let rec to_string ?(indices=false) = function
  | Var (id, idx) -> if indices then Int.to_string idx else id
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
