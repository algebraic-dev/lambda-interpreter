open Compiler.Ast;
open Compiler.Expr;
open Compiler.Compute;

let () =
  if (Array.length(Sys.argv) <= 1) {
    print_endline("Wrong usage, specify the file plz");
  } else {
    let ls = Compiler.Reader.read_from_file(Sys.argv[1]);
    let (equivalences, computation) = ast_to_expr(ls);
    let reduce = List.map(((name, formula)) => (name, compute(formula)));
    computation
    |> barengdt
    |> compute
    |> show_expr(equivalences |> reduce)
    |> print_endline;
  };
