open! Opl
               (* p    a     b *)
let test_subst ~name ~with' ~in' =
  let with' = Result.get_ok (Lambda.of_string with') in
  let in' = Result.get_ok (Lambda.of_string in') in
  Lambda.subst ~name ~with' ~in' |> Lambda.to_string
                                 |> Printf.printf "%s\n"
;;

let%expect_test "variable substitution" = 
  test_subst ~name:"x" ~with':"y" ~in':"x"; [%expect "y"];
  test_subst ~name:"y" ~with':"z" ~in':"x"; [%expect "x"];
  test_subst ~name:"a" ~with':"z" ~in':"a"; [%expect "z"];
  test_subst ~name:"y" ~with':"x" ~in':{| \x.x |}; [%expect {| λx.x |}];
  test_subst ~name:"z" ~with':"y" ~in':{| \x.(z x) |}; [%expect {| λx.(y x) |}];
  test_subst ~name:"a" ~with':"b" ~in':"(a b)"; [%expect "(b b)"];
;;

let%expect_test "capture avoiding substitution" =
  (* THIS SHOULD BE UNDIFINED *)
  test_subst ~name:"z" ~with':{| \y.(x (y y)) |} ~in':{| \y.(y z) |}; [%expect {| λy.(y λy.(x (y y))) |}];

  test_subst ~name:"z" ~with':{| \y.(x (y y)) |} ~in':{| \y.(y z) |};
  [%expect {| λy.(y λy.(x (y y))) |}]
;;

