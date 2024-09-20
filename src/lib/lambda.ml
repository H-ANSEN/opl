type t =
  | Var of identifier
  | Lambda of identifier * t 
  | Application of t * t 

and identifier = string

(** TODO rework parser to provide better errors this may need to be implemented
    at the 'Parsers' module *)
module Parser = struct
  open Parsers
  open Parsers.Let_syntax

  let string_of_chars chars = chars |> List.to_seq |> String.of_seq

  let rec parse input =
    let p = space *> (var <|> lambda <|> application) in
    p input

  and var input =
    let p = map identifier ~f:(fun var -> Var var) in 
    p input

  and lambda input =
    let p =
      let* param = charp '\\' *> space *> identifier <* space
      and+ body = charp '.' *> space *> parse in
      return (Lambda (param, body))
    in
    p input

  and application input =
    let p =
      let* callee = charp '(' *> space *> parse <* space
      and+ argument = parse <* space <* charp ')' in
      return (Application (callee, argument))
    in
    p input

  and space =
    many (charp ' ' <|> charp '\t' <|> charp '\n' <|> charp '\r')

  and identifier =
    let* start = alpha
    and+ rest = many (alpha <|> digit) in
    return (string_of_chars (start :: rest))
end

let rec to_string = function
  | Var ident -> ident
  | Lambda (param, body) -> Printf.sprintf "λ%s.%s" param (to_string body)
  | Application (exp1, exp2) -> Printf.sprintf "(%s %s)" (to_string exp1) (to_string exp2)

let of_string str =
  let input = Parsers.Input.make str in
  Result.map (fun (result, _) -> result) (Parser.parse input)
