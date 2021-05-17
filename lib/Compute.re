open Expr;

let get_new_name = (name, list) => {
  let rec loop = (name, list) =>
    if (List.exists((==)(name), list)) {
      loop(name ++ "'", list);
    } else {
      name;
    };
  loop(name, list);
};

let barengdt = ast => {
  let rec loop = (fv, ast) => {
    switch (ast) {
    | Var(name) => Var(get_new_name(name, fv))
    | App(func, arg) =>
      let fixed_func = loop(fv, func);
      let new_fv = rem_duplicated(fv, get_variables(fixed_func));
      let fixed_arg = loop(new_fv, arg);
      App(fixed_func, fixed_arg);
    | Lambda(name, arg) =>
      let new_name = get_new_name(name, fv);
      let fv = new_name == name ? [name, ...fv] : fv;
      Lambda(new_name, loop(fv, arg));
    };
  };
  loop([], ast);
};

let rec substitute = (body, name, subs) => {
  switch (body) {
  | Var(var_name) when name == var_name => subs
  | Var(name) => Var(name)
  | App(func, arg) =>
    App(substitute(func, name, subs), substitute(arg, name, subs))
  | Lambda(func_name, body) when func_name == name => Lambda(func_name, body)
  | Lambda(func_name, body) =>
    Lambda(func_name, substitute(body, name, subs))
  };
};

let rec beta_reduction = expr => {
  switch (expr) {
  | Lambda(name, body_expr) => Lambda(name, beta_reduction(body_expr))
  | App(Lambda(name, expr), arg) =>
    substitute(beta_reduction(expr), name, beta_reduction(arg))
  | App(func, arg) => App(beta_reduction(func), beta_reduction(arg))
  | other => other
  };
};
