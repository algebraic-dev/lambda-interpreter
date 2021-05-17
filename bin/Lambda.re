open Compiler.Ast;
open Compiler.Expr;

let () = {
  let ast =
    Compiler.Reader.read_from_file("./test_suite/false.lambda")
    |> ast_to_expr
    |> Compiler.Compute.barengdt
    |> Compiler.Compute.compute;
  ast |> show_expr |> print_endline;
};
