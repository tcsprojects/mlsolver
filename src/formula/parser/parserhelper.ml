open Parser;;
open Lexer;;
open Printf;;
open Metaformula;;

let parse s =
  let buff = Lexing.from_string(s)
in
  try
    Parser.program Lexer.lexer buff
  with
    Failure("lexing: empty token") -> print_string "Parser failure :: Empty Token\n"; exit 0;
  | Lexer.Eof -> print_string "Parser failure :: End Of String\n"; exit 0;;

let parse_expression s =
  match (parse ("#dummydef := " ^ s ^ ";")) with
    ("#dummydef", [], [(CondTT, expr)])::[] -> expr
  | _ -> failwith("String '" ^ s ^ "' is not an expression!");;