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

let%expect_test "lambda abstraction with var expression" =
  test_bb {| \x.x |}; [%expect {| λx.x |}];
  test_bb {| \a.b |}; [%expect {| λa.b |}];
  test_bb {| \a.t |}; [%expect {| λa.t |}];
  test_bb {| \a.a |}; [%expect {| λa.a |}];
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

let%expect_test "sequence of variables is expanded" =
  test_bb {| \xyz.n |}; [%expect {| λx.λy.λz.n |}];
  test_bb {| \xyz.(x (y z)) |}; [%expect {| λx.λy.λz.(x (y z)) |}];
  test_bb {| (z \ab.n) |}; [%expect {| (z λa.λb.n) |}];
  test_bb {| \ab.\cd.\ef.g |}; [%expect {| λa.λb.λc.λd.λe.λf.g |}];
;;
