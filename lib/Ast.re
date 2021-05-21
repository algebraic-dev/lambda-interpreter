open Expr;

type ast =
  | Variable(string)
  | Application(ast, ast)
  | Abstraction(string, ast)
  | Substitution(string);

type ast_expr =
  | Ast(ast)
  | Let(string, ast);

let rec substitute = (name, ast, bindings) =>
  switch (ast) {
  | Application(func, arg) =>
    let func' = substitute(name, func, bindings);
    let arg' = substitute(name, arg, bindings);
    App(func', arg');
  | Abstraction(fun_name, body) =>
    Lambda(fun_name, substitute(name, body, bindings))
  | Substitution(sub_name) =>
    if (sub_name == name) {
      failwith("Cannot make let recursivity '" ++ name ++ "'");
    } else {
      try(substitute(sub_name, Hashtbl.find(bindings, sub_name), bindings)) {
      | Not_found => failwith("Cannot find function " ++ sub_name)
      };
    }
  | Variable(a) => Var(a)
  };

let ast_to_expr = list => {
  let bindings = Hashtbl.create(0);
  let rec loop = list_tail => {
    switch (list_tail) {
    | [Let(name, expr), ...tl] =>
      Hashtbl.add(bindings, name, expr);
      let loop_res = loop(tl);
      name == "Main" ? Some(expr) : loop_res;
    | [Ast(ast), ...tl] =>
      switch (loop(tl)) {
      | None => Some(ast)
      | Some(_) as res => res
      }
    | [] => None
    };
  };
  let main = loop(list);
  switch (main) {
  | None => failwith("Cannot find entrypoint")
  | Some(entrypoint) => (bindings, substitute("Main", entrypoint, bindings))
  };
};
