open Opl

let () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Usage %s [program]\n" Sys.argv.(0);
    exit 1
  );
  let program = Sys.argv.(1) in
  match Lambda.of_string program with
  | Ok ast -> Printf.printf "%s\n" (Lambda.to_string ast)
  | Error err -> Printf.printf "%s\n" (Parsers.Error.to_string err)
