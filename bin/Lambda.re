open Compiler.Ast;
open Compiler.Expr;

let () = {
  let ast = Compiler.Reader.read_from_file("./test_suite/false.lambda");
  let (ctx, expr) = ast_to_expr(ast);
  let expr = Compiler.Compute.barengdt(expr);
  let rec loop = last => {
    let computed = Compiler.Compute.beta_reduction(last);
    if (Compiler.Expr.equal(last, computed)) {
      computed;
    } else {
      loop(computed);
    };
  };
  loop(expr) |> show_expr(ctx) |> print_endline;
};
