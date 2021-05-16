module Ast = Ast;
open Ast;

type context = {bindings: Hashtbl.t(string, ast)};

let create_context = list => {
  List.fold_left(
    (ctx, expr) => {
      switch (expr) {
      | Let(name, ast) => Hashtbl.add(ctx.bindings, name, ast)
      | _ => ()
      };
      ctx;
    },
    {bindings: Hashtbl.create(0)},
    list,
  );
};

let get_binding = (ctx, name) =>
  try(Hashtbl.find(ctx.bindings, name)) {
  | Not_found => failwith("Not found " ++ name)
  };

let rec free_variables = (ctx, ast) => {
  switch (ast) {
  | Subs(x) => free_variables(ctx, get_binding(ctx, x))
  | Variable(x) => [x]
  | Application(a, b) =>
    List.append(free_variables(ctx, a), free_variables(ctx, b))
  | Abstraction(name, body) =>
    List.filter((!=)(name), free_variables(ctx, body))
  };
};

let rec equal_expr = (ctx, a, b) =>
  switch (a, b) {
  | (Variable(_), Variable(_)) => true
  | (Abstraction(_, ast), Abstraction(_, ast2)) =>
    equal_expr(ctx, ast, ast2)
  | (Application(exprA1, exprA2), Application(exprB1, exprB2)) =>
    equal_expr(ctx, exprA1, exprB1) && equal_expr(ctx, exprA2, exprB2)
  | (Subs(a), expr)
  | (expr, Subs(a)) =>
    let expr_subs = get_binding(ctx, a);
    equal_expr(ctx, expr_subs, expr);
  | (_, _) => false
  };
