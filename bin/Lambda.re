open Compiler.Ast;
open Compiler.Expr;
open Compiler.Compute;

let () =
  if (Array.length(Sys.argv) <= 1) {
    print_endline("Wrong usage, specify the file plz");
  } else {
    Compiler.Reader.read_from_file(Sys.argv[1])
    |> ast_to_expr
    |> barengdt
    |> compute
    |> show_expr
    |> print_endline;
  };
