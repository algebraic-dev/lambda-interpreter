open Compiler.Ast;
open Compiler.Expr;
open Compiler.Compute;

let () = {
  Compiler.Reader.read_from_file("./test_suite/false.lambda")
  |> ast_to_expr
  |> barengdt
  |> compute
  |> show_expr
  |> print_endline;
};
