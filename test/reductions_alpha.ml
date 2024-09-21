open! Opl

let test_a_conv ~name str =
  Printf.printf "%s\n"
    (match Lambda.of_string str with
     | Error err -> Parsers.Error.to_string err
     | Ok ast ->
       let conv = Reductions.alpha_conversion ~name ast in
       if Option.is_none conv then "Alpha-conversion test unexpected FAIL!!"
       else conv |> Option.get |> Lambda.to_string)
;;

let%expect_test "simple alpha-conversion" =
  test_a_conv ~name:"a" {| \x.x |}; [%expect {| λa.a |}];
  test_a_conv ~name:"a" {| \x.(x x) |}; [%expect {| λa.(a a) |}];
  test_a_conv ~name:"y" {| \x.\x.x |}; [%expect {| λy.λx.x |}];
  test_a_conv ~name:"t" {| \y.\z.(\y.z y) |}; [%expect {| λt.λz.(λy.z t) |}];
  test_a_conv ~name:"q" {| \y.((x z) (\x.(x y) \x.x)) |}; [%expect {| λq.((x z) (λx.(x q) λx.x)) |}];
;;
