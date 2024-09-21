open! Opl

(** Tests checking for the proper identification of free and bound variables
    within an expression.

    see https://en.wikipedia.org/wiki/Lambda_calculus#Free_and_bound_variables
    which provides a formal definition for what defines a variable as free or
    bound.
*)

let test_free str =
  match Lambda.of_string str with
  | Error err -> Printf.printf "%s\n" (Parsers.Error.to_string err)
  | Ok ast ->
    ast
    |> Lambda.free_vars
    |> Lambda.IdentifierSet.elements
    |> List.iter (fun var -> Printf.printf "%s " var)
;;

let test_bound str =
  match Lambda.of_string str with
  | Error err -> Printf.printf "%s\n" (Parsers.Error.to_string err)
  | Ok ast ->
    ast
    |> Lambda.bound_vars
    |> Lambda.IdentifierSet.elements
    |> List.iter (fun var -> Printf.printf "%s " var)
;;

let%expect_test "test free variables identification" =
  test_free {| (\v.\v.z \y.\u.y) |}; [%expect {| z |}];
  test_free {| ((((v u) y) ((z z) \u.u)) (\x.x (v v))) |}; [%expect {| u v y z |}];
  test_free {| (\v.\z.z \x.\u.u) |}; [%expect {| |}];
  test_free {| (\y.(x \v.v) \x.(x x)) |}; [%expect {| x |}];
  test_free {| (((\z.z \x.v) \y.\v.y) \y.\z.z) |}; [%expect {| v |}];
  test_free {| (\y.((y y) x) ((z y) \x.x)) |}; [%expect {| x y z |}];
  test_free {| (\v.\u.(u u) \x.(x v)) |}; [%expect {| v |}];
  test_free {| ((((u y) \u.v) \x.\z.z) (\v.v \v.y)) |}; [%expect {| u v y |}];
;;

let%expect_test "test free variables with contracted abstractions" =
  test_free {| (\zvz.v \u.(y x)) |}; [%expect {| x y |}];
  test_free {| (\y.((x z) (z y)) \vx.y) |}; [%expect {| x y z |}];
  test_free {| (\zu.(u u) (\v.x \z.u)) |}; [%expect {| u x |}];
;;

let%expect_test "test bound variables identification" =
  test_bound {| \x.x |}; [%expect {| x |}];
  test_bound {| (\x.x y) |}; [%expect {| x |}];
  test_bound {| \y.(x (x y)) |}; [%expect {| y |}];
  test_bound {| (\x.((x x) z) \y.\y.((z y) \x.x)) |}; [%expect {| x y |}];
  test_bound {| (\x.\y.(y z) \y.(y x)) |}; [%expect {| x y |}];
  test_bound {| (\v.\z.z \x.\u.u) |}; [%expect {| u v x z |}];
  test_bound {| (\y.(x \v.v) \x.(x x)) |}; [%expect {| v x y |}];
  test_bound {| (((\z.z \x.v) \y.\v.y) \y.\z.z) |}; [%expect {| v x y z |}];
;; 

let%expect_test "test bound variables with contracted abstractions" =
  test_bound {| \abcd.x |}; [%expect {| a b c d |}];
  test_bound {| ((\x.x \abc.(y z)) x) |}; [%expect {| a b c x |}];
  test_bound {| (\zu.(u u) (\v.x \z.u)) |}; [%expect {| u v z |}];
;;
