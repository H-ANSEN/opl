type t =
  | Var of identifier
  | Lambda of identifier * t 
  | Application of t * t 

and identifier = string
    
module IdentifierSet = Set.Make ( String )

(** TODOO rework parser to provide better errors this may need to be implemented
    at the 'Parsers' module *)
module Parser = struct
  open Parsers
  open Parsers.Let_syntax

  let many1 p =
    let* first = p and+ rest = many p in
    return (first :: rest)

  let rec parse input =
    (space *> (var <|> lambda <|> application)) input

  and var input =
    (map identifier ~f:(fun var -> Var var)) input

  and lambda input =
    (let* vars = charp '\\' *> space *> many1 identifier <* space
     and+ body = charp '.' *> space *> parse in
     return @@ List.fold_right (fun var acc -> Lambda (var, acc)) vars body)
    input

  and application input =
    (let* callee = charp '(' *> space *> parse <* space
     and+ argument = parse <* space <* charp ')' in
     return (Application (callee, argument)))
    input

  and identifier = map alpha ~f:Char.escaped
  and space = many (charp ' ' <|> charp '\t' <|> charp '\n' <|> charp '\r')
end

let rec to_string = function
  | Var ident -> ident
  | Lambda (param, body) -> Printf.sprintf "λ%s.%s" param (to_string body)
  | Application (exp1, exp2) -> Printf.sprintf "(%s %s)" (to_string exp1) (to_string exp2)
;;

let of_string str =
  let input = Parsers.Input.make str in
  Result.map (fun (result, _) -> result) (Parser.parse input)
;;

(** see https://en.wikipedia.org/wiki/Lambda_calculus#Free_and_bound_variables
    for a formal descritpion of the algorithm used below *)
let free_vars t =
  let set = IdentifierSet.empty in
  let rec collect_free_vars set = function
    | Var ident -> IdentifierSet.add ident set
    | Lambda (ident, expr) ->
      let set1 = collect_free_vars set expr in
      IdentifierSet.remove ident set1
    | Application (exp1, exp2) ->
      let set1 = collect_free_vars set exp1 in
      let set2 = collect_free_vars set exp2 in
      IdentifierSet.union set1 set2
  in
  collect_free_vars set t
;;

let bound_vars t =
  let set = IdentifierSet.empty in
  let rec collect_bound_vars set = function
    | Var _ -> IdentifierSet.empty
    | Lambda (var, body) ->
      let body_bound = collect_bound_vars set body in
      IdentifierSet.add var body_bound
    | Application (exp1, exp2) ->
      let set1 = collect_bound_vars set exp1 in
      let set2 = collect_bound_vars set exp2 in
      IdentifierSet.union set1 set2
  in
  collect_bound_vars set t
;;
