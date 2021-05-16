open Compiler.Ast;

let is_main = a =>
  switch (a) {
  | Let(name, _) when name == "Main" => true
  | _ => false
  };

let find_main = list =>
  try(
    switch (list) {
    | [Ast(ast)] => ast
    | _ =>
      let Let(_, ast) | Ast(ast) = List.find(is_main, list);
      ast;
    }
  ) {
  | Not_found => failwith("Main not found!")
  };

let () = {
  let list = Compiler.Reader.read_from_file("./test_suite/false.lambda");

  find_main(list) |> show_ast |> print_endline;
};
