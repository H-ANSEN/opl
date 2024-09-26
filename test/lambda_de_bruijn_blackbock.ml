open! Opl

(** Psudo blackbox tests ensuring that a string value can be converted to the
    internal ast value and then back in 'de Bruijn index notation' ensuring
    correct calculation of indices.
*)

let test_dbi str =
  Printf.printf "%s\n"
    (match Lambda.of_string str with
     | Ok ast -> Lambda.to_string ~indices:true ast
     | Error err -> Parsers.Error.to_string err)
;;

let%expect_test "de bruijn notation on closed expression" =
  test_dbi {| \x.x |}; [%expect "λ 0"];
  test_dbi {| \z.z |}; [%expect "λ 0"];
  test_dbi {| \x.\y.x |}; [%expect "λ λ 1"];
  test_dbi {| (\x.\x.x \y.y) |}; [%expect "(λ λ 0 λ 0)"];
  test_dbi {| (\x.(x x) \x.(x x)) |}; [%expect "(λ (0 0) λ (0 0))"];
  test_dbi {| \x.\y.\s.\z.(x (s (y (s z))))|}; [%expect "λ λ λ λ (3 (1 (2 (1 0))))"];
;;

let%expect_test "de bruijn notation on open expression" =
  test_dbi {| \x.y |}; [%expect "λ 1"];
  test_dbi {| \x.(x y) |}; [%expect "λ (0 1)"];
  test_dbi {| \x.\y.(y z) |}; [%expect "λ λ (0 2)"];
  test_dbi {| \x.(x (y y)) |}; [%expect "λ (0 (1 1))"];
  test_dbi {| \x.(x (y z)) |}; [%expect "λ (0 (1 2))"];
  test_dbi {| \x.(a (b (a b))) |}; [%expect "λ (1 (2 (1 2)))"];
  test_dbi {| \x.(a (b (a \x.x))) |}; [%expect "λ (1 (2 (1 λ 0)))"];
  test_dbi {| (\x.(x y) \x.(x y)) |}; [%expect "(λ (0 1) λ (0 1))"];
  test_dbi {| \a.(b \c.(c d)) |}; [%expect "λ (1 λ (0 3))"]; 








  test_dbi {| \a.(b \c.(c (d \x.(x d)))) |}; [%expect {| λ (1 λ (0 (3 λ (0 3)))) |}];

  test_dbi {| \a.(e \b.(e \c.(e \d.(e d)))) |}; [%expect {| λ (1 λ (1 λ (1 λ (1 3)))) |}];
  test_dbi {| \a.(h \b.(i \c.(j \d.(k d)))) |}; [%expect {| λ (1 λ (1 λ (1 λ (1 3)))) |}];
;;
