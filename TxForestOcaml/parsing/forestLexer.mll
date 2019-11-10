{
open Lexing
open ForestParser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let codeToChar str =
  let newint = int_of_string ("0" ^ (String.sub str 1 ((String.length str) - 1))) in
  if newint > 255 || newint < 0
  then raise (SyntaxError ("Not a valid character code:" ^ str))
  else Char.chr newint

(* Stole from OCaml compiler *)
let char_for_backslash = function
    'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c
}

let white = [' ' '\t']+
let digit = [ '0'-'9' ]
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let newline = '\r' | '\n' | "\r\n"
let lid = ['a'-'z' '_'] (letter | digit | '_')*
let uid = ['A'-'Z'] (letter | digit | '_')*

let octalD = [ '0'-'7' ]
let hexaD = [ '0'-'9' 'a'-'f' ]
let asccode = digit digit? digit?
let octocode = 'o' octalD octalD? octalD?
let hexacode = 'x' hexaD hexaD?

let backslash_escapes = ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

rule forest_read =
  parse
  | white { forest_read lexbuf }
  | newline { next_line lexbuf; forest_read lexbuf }
  | "directory" { DIR}
  | "file" { FILE }
  | "option" { OPT }
  | "link" { LINK }
  | "pads" { PADS }
  | "is" { IS }
  | "matches" { MATCHES }
  | "GL" { GL }
  | "RE" { RE }
  | "where" { WHERE }
  | "(*" { read_comment 0 lexbuf }
  | "::" { DCOLON }
  | "<-" { BARROW }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | '|' { BAR }
  | '=' { EQ }
  | '$' { read_antiquot (Buffer.create 17) lexbuf }
  | lid { ID (Lexing.lexeme lexbuf) }
  | _   { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
  
and read_comment n =
  parse
  | newline { next_line lexbuf; read_comment n lexbuf }
  | "(*" { read_comment (n+1) lexbuf }
  | "*)" { if n < 1 then forest_read lexbuf
           else read_comment (n-1) lexbuf }
  | [ ^ '(' '*' ')' ] { read_comment n lexbuf }
  | _    { read_comment n lexbuf }
  | eof  { raise (SyntaxError ("Comment is not terminated")) }
  
and read_antiquot buf =
  parse
  | newline { next_line lexbuf;
              Buffer.add_string buf (Lexing.lexeme lexbuf);
              read_antiquot buf lexbuf }
  | '$'       { AQUOT (Buffer.contents buf) }
  | [^ '$' '\n' '\r']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_antiquot buf lexbuf
    }
  | _ { raise (SyntaxError ("Antiquotation failure at: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("Antiquotation is not terminated")) }
  
and read_string buf =
  parse
  | newline { next_line lexbuf;
              Buffer.add_string buf (Lexing.lexeme lexbuf);
              read_string buf lexbuf }
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' backslash_escapes
      { Buffer.add_char buf (char_for_backslash (Lexing.lexeme_char lexbuf 1));
        read_string buf lexbuf }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' asccode   { Buffer.add_char buf (codeToChar (lexeme lexbuf)) ; read_string buf lexbuf }
  | '\\' octocode  { Buffer.add_char buf (codeToChar (lexeme lexbuf)) ; read_string buf lexbuf }
  | '\\' hexacode  { Buffer.add_char buf (codeToChar (lexeme lexbuf)) ; read_string buf lexbuf }
  | [^ '"' '\\' '\n' '\r']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
   }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }



