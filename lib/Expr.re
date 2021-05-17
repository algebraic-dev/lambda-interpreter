open Printf;

type expr =
  | Var(string)
  | App(expr, expr)
  | Lambda(string, expr);

let rec show_expr =
  fun
  | Var(x) => x
  | App(Lambda(_) as func, Lambda(_) as arg) =>
    sprintf("(%s) (%s)", show_expr(func), show_expr(arg))
  | App(Lambda(_) as func, arg) =>
    sprintf("(%s) %s", show_expr(func), show_expr(arg))
  | App(func, Lambda(_) as arg) =>
    sprintf("%s (%s)", show_expr(func), show_expr(arg))
  | App(func, arg) => sprintf("%s %s", show_expr(func), show_expr(arg))
  | Lambda(name, expr) => sprintf("λ%s. %s", name, show_expr(expr));

let rec free_variables =
  fun
  | Var(x) => [x]
  | App(func, arg) =>
    List.append(free_variables(func), free_variables(arg))
  | Lambda(name, expr) => List.filter((!=)(name), free_variables(expr));

let rec rem_duplicated = (a, b) => {
  switch (a) {
  | [element, ...tail] =>
    let res = List.exists((==)(element), b) ? b : [element, ...b];
    rem_duplicated(tail, res);
  | [] => b
  };
};

let rec get_variables =
  fun
  | Var(x) => [x]
  | App(func, arg) =>
    rem_duplicated(get_variables(func), get_variables(arg))
  | Lambda(name, expr) => rem_duplicated([name], get_variables(expr));
