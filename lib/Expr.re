open Printf;

type expr =
  | Var(string)
  | App(expr, expr)
  | Lambda(string, expr);

let rec is_alpha_equivalent = (a, b) =>
  switch (a, b) {
  | (Var(_), Var(_)) => true
  | (App(func, arg), App(func', arg')) =>
    is_alpha_equivalent(func, func') && is_alpha_equivalent(arg, arg')
  | (Lambda(_, expr), Lambda(_, expr')) => is_alpha_equivalent(expr, expr')
  | _ => false
  };

let rec expr_to_str = eqs =>
  fun
  | Var(x) => x
  | App(Lambda(_) as func, Lambda(_) as arg) =>
    sprintf("(%s) (%s)", show_expr(eqs, func), show_expr(eqs, arg))
  | App(Var(x), Var(y)) => sprintf("(%s %s)", x, y)
  | App(Lambda(_) as func, arg) =>
    sprintf("(%s) %s", show_expr(eqs, func), show_expr(eqs, arg))
  | App(func, Lambda(_) as arg) =>
    sprintf("%s (%s)", show_expr(eqs, func), show_expr(eqs, arg))
  | App(func, arg) =>
    sprintf("(%s %s)", show_expr(eqs, func), show_expr(eqs, arg))
  | Lambda(name, expr) => sprintf("λ%s. %s", name, show_expr(eqs, expr))

and show_expr = (eqs, formula) =>
  try(
    fst @@
    List.find(
      ((name, eq_formula)) => {
        name != "Main" && is_alpha_equivalent(formula, eq_formula)
      },
      eqs,
    )
  ) {
  | Not_found => expr_to_str(eqs, formula)
  };

let rec rem_duplicated = (a, b) => {
  switch (a) {
  | [element, ...tail] =>
    let res = List.exists((==)(element), b) ? b : [element, ...b];
    rem_duplicated(tail, res);
  | [] => b
  };
};
