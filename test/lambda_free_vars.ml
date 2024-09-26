open! Opl

let test_free str =
  match Lambda.of_string str with
  | Error err -> Printf.printf "%s\n" (Parsers.Error.to_string err)
  | Ok ast ->
    ast |> Lambda.free_vars
        |> Lambda.IdentifierSet.elements
        |> List.iter (fun var -> Printf.printf "%s " var)
;;

let%expect_test "test free variables identification" =
  test_free {| x |}; [%expect {| x |}];
  test_free {| (a b) |}; [%expect {| a b |}];
  test_free {| \x.(x y) |}; [%expect {| y |}];
  test_free {| \a.\b.(a (b z)) |}; [%expect {| z |}];
  test_free {| (\v.\v.z \y.\u.y) |}; [%expect {| z |}];
  test_free {| ((((v u) y) ((z z) \u.u)) (\x.x (v v))) |}; [%expect {| u v y z |}];
  test_free {| (\v.\z.z \x.\u.u) |}; [%expect {| |}];
  test_free {| (\y.(x \v.v) \x.(x x)) |}; [%expect {| x |}];
  test_free {| (((\z.z \x.v) \y.\v.y) \y.\z.z) |}; [%expect {| v |}];
  test_free {| (\y.((y y) x) ((z y) \x.x)) |}; [%expect {| x y z |}];
  test_free {| (\v.\u.(u u) \x.(x v)) |}; [%expect {| v |}];
  test_free {| ((((u y) \u.v) \x.\z.z) (\v.v \v.y)) |}; [%expect {| u v y |}];
;;
