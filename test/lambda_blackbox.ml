open Opl

(** Psudo black box tests ensuring that a string value can be converted to the
    internal ast value and then back into the "same" string.

    <λexp> ::= <var>
             | λ <var> . <λexp>
             | ( <λexp> <λexp> )
*)

let test_bb str =
  Printf.printf "%s\n"
    (match Lambda.of_string str with
     | Ok ast -> Lambda.to_string ast
     | Error err -> Parsers.Error.to_string err)
;;

let%expect_test "single character var test" =
  test_bb "x"; [%expect {| x |}];
  test_bb "a"; [%expect {| a |}];
  test_bb "M"; [%expect {| M |}];
;;

let%expect_test "multi-character var test" =
  test_bb "test"; [%expect {| test |}];
  test_bb "az"; [%expect {| az |}];
  test_bb "Bx"; [%expect {| Bx |}];
  test_bb "zL"; [%expect {| zL |}];
  test_bb "reallyLongVariableName"; [%expect {| reallyLongVariableName |}];
;;

let%expect_test "alpha-numeric var test" =
  test_bb "x1234567890"; [%expect {| x1234567890 |}];
  test_bb "test0"; [%expect {| test0 |}];
  test_bb "a1b2c3"; [%expect {| a1b2c3 |}];
  test_bb "t0"; [%expect {| t0 |}];
;;

let%expect_test "lambda abstraction with var expression" =
  test_bb {| \x.x |}; [%expect {| λx.x |}];
  test_bb {| \a.b |}; [%expect {| λa.b |}];
  test_bb {| \a.testing |}; [%expect {| λa.testing |}];
  test_bb {| \a1.a2 |}; [%expect {| λa1.a2 |}];
;;

let%expect_test "lambda abstraction with application" =
  test_bb {| \x.(x x) |}; [%expect {| λx.(x x) |}];
  test_bb {| \x.(x y) |}; [%expect {| λx.(x y) |}];
  test_bb {| \a.(b c) |}; [%expect {| λa.(b c) |}];
  test_bb {| \a.(a a) |}; [%expect {| λa.(a a) |}];
;;

let%expect_test "nested lambda abstraction" =
  test_bb {| \x.\y.(x y) |}; [%expect {| λx.λy.(x y) |}];
  test_bb {| \a.\b.a |}; [%expect {| λa.λb.a |}];
  test_bb {| \x.\y.\z.(x y) |}; [%expect {| λx.λy.λz.(x y) |}];
  test_bb {| \x.\y.\z.\x.(y z) |}; [%expect {| λx.λy.λz.λx.(y z) |}];
;;

let%expect_test "lambda abstraction with multi-level application" =
  test_bb {| ((z \x.\y.z) (x y)) |}; [%expect {| ((z λx.λy.z) (x y)) |}];
  test_bb {| (\v.\y.(\u.x y) \z.(\v.(v z) z)) |}; [%expect {| (λv.λy.(λu.x y) λz.(λv.(v z) z)) |}];
  test_bb {| \u.(\z.\v.(z z) \z.(\x.z u)) |}; [%expect {| λu.(λz.λv.(z z) λz.(λx.z u)) |}];
  test_bb {| (\u.\v.((v u) (v y)) (\v.z (v \y.(y y)))) |}; [%expect {| (λu.λv.((v u) (v y)) (λv.z (v λy.(y y)))) |}];
  test_bb {| \x.\v.((v (v x)) \v.\z.x) |}; [%expect {| λx.λv.((v (v x)) λv.λz.x) |}];
;;
