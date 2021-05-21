open Expr;

type ast =
  | Variable(string)
  | Application(ast, ast)
  | Abstraction(string, ast)
  | Substitution(string);

type ast_expr =
  | Ast(ast)
  | Let(string, ast);

let ast_to_expr: list(ast_expr) => (Hashtbl.t(string, ast), expr);
