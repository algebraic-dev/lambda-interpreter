open Expr;

let get_new_name = (name, list) => {
  let rec loop = (name, list) =>
    if (List.exists((==)(name), list)) {
      let endl = String.split_on_char('\'', name);
      switch (endl) {
      | [start, endl] =>
        let num = int_of_string(endl) + 1;
        loop(start ++ "'" ++ string_of_int(num), list);
      | [name] => loop(name ++ "'0", list)
      | _ => failwith("Impossible ")
      };
    } else {
      name;
    };
  loop(name, list);
};

let alpha_convert = (from, new_name, ast) => {
  let rec loop = ast => {
    switch (ast) {
    | Var(name) when name == from => Var(new_name)
    | App(func, arg) => App(loop(func), loop(arg))
    // Shadowing
    | Lambda(var_name, _) when var_name == new_name => ast
    | Var(_) => ast
    // Continue
    | Lambda(var_name, body) =>
      Lambda(var_name == from ? new_name : var_name, loop(body))
    };
  };
  loop(ast);
};

let barengdt = ast => {
  let rec loop = (ast, names) => {
    switch (ast) {
    | App(func, arg) =>
      let (func, names) = loop(func, names);
      let (arg, names) = loop(arg, names);
      (App(func, arg), names);
    | Lambda(var_name, body) =>
      switch (List.exists((==)(var_name), names)) {
      | true =>
        let new_name = get_new_name(var_name, names);
        let new_names = [new_name, ...names];
        let converted = alpha_convert(var_name, new_name, body);
        let (body, names) = loop(converted, new_names);
        (Lambda(new_name, body), names);
      | _ =>
        let new_names = [var_name, ...names];
        let (body, names) = loop(body, new_names);
        (Lambda(var_name, body), names);
      }
    | _ => (ast, names)
    };
  };
  fst @@ loop(ast, []);
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

// This is equivalent of a zero-or-more-steps computation
let rec compute = last => {
  let computed = beta_reduction(last);
  switch (is_alpha_equivalent(last, computed)) {
  | true => computed
  | _ => compute(computed)
  };
};
