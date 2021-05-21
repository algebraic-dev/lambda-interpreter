open Compiler.Ast;
open Compiler.Expr;

let () = {
  let rec compute = last => {
    let computed = Compiler.Compute.beta_reduction(last);
    switch (Compiler.Expr.equal(last, computed)) {
    | true => computed
    | _ => compute(computed)
    };
  };
  Compiler.Reader.read_from_file("./test_suite/false.lambda")
  |> ast_to_expr
  |> Compiler.Compute.barengdt
  |> compute
  |> show_expr
  |> print_endline;
};
