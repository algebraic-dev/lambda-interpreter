open Lexing;
open Sedlexing.Latin1;

let pos_to_string = lexbuf => {
  let pos = lexbuf.lex_curr_p;
  Printf.sprintf("%d:%d", pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1);
};

let read_from_file = str => {
  let ic = open_in(str);
  let lexbuf = from_channel(ic);
  let revised_lexer = Sedlexing.with_tokenizer(Lexer.lex, lexbuf);
  let revised_parser =
    MenhirLib.Convert.Simplified.traditional2revised(Parser.main);
  try(revised_parser(revised_lexer)) {
  | Parser.Error => failwith("Syntax error on " ++ str)
  };
};
