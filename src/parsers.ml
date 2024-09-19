module Input = struct
  type t = {
    pos : int;
    len : int;
    str : string;
  }

  let make str = { pos = 0; len = String.length str; str }
  let incr t = { t with pos = t.pos + 1 }

  let to_string t =
    if t.pos >= t.len then ""
    else String.sub t.str t.pos (t.len - t.pos)
  ;;
end

module Error = struct
  type t = {
    kind : kind;
    input : Input.t;
  }

  and kind =
    | Custom of string
    | Unexpected_end_of_input
    | Unexpected_char of char
    | Expected_but_found of { expected : char; found : char; }

  let make kind input = { kind; input }

  let to_string { kind; _ } =
    match kind with
    | Custom desc -> desc
    | Unexpected_end_of_input -> "Unexpected end of input"
    | Unexpected_char c -> Printf.sprintf "Unexpected character '%c'" c
    | Expected_but_found { expected; found } ->
      Printf.sprintf "Expected '%c' but found '%c'" expected found
end

type 'a result = ('a * Input.t, Error.t) Result.t
type 'a t = Input.t -> 'a result

let return (value : 'a) : 'a t =
  fun input -> Ok (value, input)
;;

let fail (kind : Error.kind) : 'a t =
  fun input -> Error (Error.make kind input)
;;

let map (parser : 'a t) ~(f : 'a -> 'b) : 'b t =
  fun input ->
    match parser input with
    | Error e -> Error e
    | Ok (result, rest) -> Ok (f result, rest)
;;

let bind (parser : 'a t) ~(f : 'a -> 'b t) =
  fun input ->
    match parser input with
    | Error e -> Error e
    | Ok (result, rest) -> (f result) rest
;;

let ( *> ) (parserA : 'a t) (parserB : 'b t) : 'b t =
  bind parserA ~f:(fun _ -> parserB)
;;

let ( <* ) (parserA : 'a t) (parserB : 'b t) : 'a t =
  bind parserA ~f:(fun resultA ->
    map parserB ~f:(fun _ -> resultA)
  )
;;

let ( <*> ) (parserA : 'a t) (parserB : 'b t) : ('a * 'b) t =
  bind parserA ~f:(fun resultA ->
    map parserB ~f:(fun resultB -> (resultA, resultB))
  )
;;

let ( <|> ) (parserA : 'a t) (parserB : 'a t) : 'a t =
  fun input ->
    match parserA input with
    | Error _ -> parserB input
    | Ok result -> Ok result
;;

let rec many (parser : 'a t) : 'a list t =
  fun input ->
    match parser input with
    | Error _ -> Ok ([], input)
    | Ok (result, rest) ->
      match many parser rest with
      | Ok (results, final_rest) -> Ok (result :: results, final_rest)
      | Error _ -> Ok ([result], rest)
;;

let notp (parser : 'a t) : unit t =
  fun input ->
    match parser input with
    | Error _ -> Ok ((), input)
    | Ok _ -> Error (Error.make (Custom "Negaition failure") input)
;;

let anycharp : char t =
  fun input ->
    if input.pos < input.len then
      let c = String.get input.str input.pos in
      let next = Input.incr input in
      Ok (c, next)
    else
      Error (Error.make Unexpected_end_of_input input)
;;

let charp (expected : char) : char t =
  bind anycharp ~f:(function
    | c when Char.equal c expected -> return c
    | c -> fail (Expected_but_found {expected; found=c}))
;;

let digit : char t =
  bind anycharp ~f:(function
    | '0'..'9' as c -> return c
    | c -> fail (Custom (Printf.sprintf "Expected digit character but found '%c'" c)))
;;

let alpha : char t =
  bind anycharp ~f:(function
    | 'a'..'z' | 'A'..'Z' as c -> return c
    | c -> fail (Custom (Printf.sprintf "Expected digit character but found '%c'" c)))
;;

module Monad_infix = struct
  let ( >>| ) (parser : 'a t) (f : 'a -> 'b) : 'b t =
    map parser ~f
  ;;

  let ( >>= ) (parser : 'a t) (f : 'a -> 'b t) : 'b t =
    bind parser ~f
  ;;
end

module Let_syntax = struct
  let ( let+ ) (parser : 'a t) (f : 'a -> 'b) : 'b t =
    map parser ~f
  ;;

  let ( let* ) (parser : 'a t) (f : 'a -> 'b t) : 'b t =
    bind parser ~f
  ;;

  let ( and+ ) (parserA : 'a t) (parserB : 'b t) : ('a * 'b) t =
    parserA <*> parserB
  ;;
end
