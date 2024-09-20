open Opl

(** Psudo black box tests ensuring that a string value can be converted to the
    internal ast value and then back into the same string *)

let%expect_test "bb identity test" =
  let ast = Result.get_ok (Lambda.of_string {|\x.x|}) in
  Printf.printf "%s\n" (Lambda.to_string ast);
  [%expect {| λx.x |}]
