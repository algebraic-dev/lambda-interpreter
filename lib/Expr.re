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
  | App(Var(x), Var(y)) => sprintf("(%s %s)", x, y)
  | App(Lambda(_) as func, arg) =>
    sprintf("(%s) %s", show_expr(func), show_expr(arg))
  | App(func, Lambda(_) as arg) =>
    sprintf("%s (%s)", show_expr(func), show_expr(arg))
  | App(func, arg) => sprintf("(%s %s)", show_expr(func), show_expr(arg))
  | Lambda(name, expr) => sprintf("λ%s. %s", name, show_expr(expr));

let rec rem_duplicated = (a, b) => {
  switch (a) {
  | [element, ...tail] =>
    let res = List.exists((==)(element), b) ? b : [element, ...b];
    rem_duplicated(tail, res);
  | [] => b
  };
};

let rec equal = (a, b) =>
  switch (a, b) {
  | (Var(_), Var(_)) => true
  | (App(func, arg), App(func', arg')) =>
    equal(func, func') && equal(arg, arg')
  | (Lambda(_, expr), Lambda(_, expr')) => equal(expr, expr')
  | _ => false
  };
