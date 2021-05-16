type ast =
  | Variable(string)
  | Application(ast, ast)
  | Abstraction(string, ast)
  | Subs(string);

type expr =
  | Ast(ast)
  | Let(string, ast);

let rec show_ast =
  fun
  | Variable(str)
  | Subs(str) => str
  | Abstraction(name, ast) => "λ" ++ name ++ "." ++ show_ast(ast)
  | Application(Abstraction(_, _) as ast1, ast2) =>
    "( " ++ show_ast(ast1) ++ " ) " ++ show_ast(ast2)
  | Application(ast1, Abstraction(_, _) as ast2) =>
    show_ast(ast1) ++ " ( " ++ show_ast(ast2) ++ " )"
  | Application(ast1, ast2) => show_ast(ast1) ++ " " ++ show_ast(ast2);

let show_expr =
  fun
  | Let(str, ast) => str ++ " = " ++ show_ast(ast)
  | Ast(x) => show_ast(x);
