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

let%expect_test "simple" =
  test_dbi {| \t.(u t) |}; [%expect {| λ (u 0) |}];
  test_dbi {| (\x.x x) |}; [%expect {| (λ 0 x) |}];
  test_dbi {| \x.\y.x |}; [%expect {| λ λ 1 |}];
  test_dbi {| \x.\y.\z.(x (z (y z))) |}; [%expect {| λ λ λ (2 (0 (1 0))) |}];
;;
