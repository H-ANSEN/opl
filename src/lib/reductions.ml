open Lambda

(** TODOO alpha conversion on application when first expresson of application is
    a lambda expression *)
let alpha_conversion ~name = function
  | Var _ | Application _ -> None
  | Lambda (param, _) as expr when param = name -> Some expr
  | Lambda (param, body) as expr ->
    if IdentifierSet.mem name (free_vars expr) then None
    else
      let rec rename param name = function
        | Var id when id = param -> Var name
        | Var _ as v -> v
        | Lambda (id, _) as l when id = param -> l
        | Lambda (id, body) -> Lambda (id, rename param name body)
        | Application (t0, t1) -> Application (rename param name t0, rename param name t1)
      in
      Some (Lambda (name, rename param name body))
;;
