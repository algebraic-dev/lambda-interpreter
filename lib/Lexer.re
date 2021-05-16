open Parser;
open Sedlexing.Utf8;

let subsName = [%sedlex.regexp?
  Compl(' ' | '.' | '=' | '(' | ')' | ';' | '\n' | '\t' | '\r')
];

let rec lex = lexbuf => {
  switch%sedlex (lexbuf) {
  | Plus("\r" | "\n") => lex(lexbuf)
  | "\t"
  | " " => lex(lexbuf)
  | "λ" => LAMBDA
  | "(" => LPAR
  | ")" => RPAR
  | "=" => EQUAL
  | "." => DOT
  | "let" => LET
  | ('a' .. 'z', Star('\'')) => VAR(lexeme(lexbuf))
  | Plus(subsName) => NAME(lexeme(lexbuf))
  | eof => EOF
  | any => failwith("Unexpected char '" ++ lexeme(lexbuf) ++ "\'")
  | _ => failwith("Unreachable error.")
  };
};
