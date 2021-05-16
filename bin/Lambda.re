open Compiler.Ast;
open Compiler.Expr;

let () = {
  let ast =
    Compiler.Reader.read_from_file("./test_suite/false.lambda") |> ast_to_expr;
  ast |> show_expr |> print_endline;
  ast |> free_variables |> List.length |> print_int;
};
