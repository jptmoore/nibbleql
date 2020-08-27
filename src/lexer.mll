{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}


let int = '-'? ['0'-'9'] ['0'-'9']*


let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?


let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"


rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | ';'      { SEMI_COLON }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "SET" | "set"  { SET }
  | "POST" | "post"  { POST }
  | "TO" | "to"  { TO }
  | "WHERE" | "where" { WHERE }
  | "IS" | "is" { IS }
  | "HOST" | "host" { HOST }
  | "GET" | "get" { GET }
  | "DELETE" | "delete" { DELETE }
  | "LAST" | "last" { LAST }
  | "FROM" | "from" { FROM }
  | "SINCE" | "since" { SINCE }
  | "RANGE" | "range" { RANGE }
  | "MIN" | "min" { MIN }
  | "MAX" | "max" { MAX }
  | "SUM" | "sum" { SUM }
  | "COUNT" | "count" { COUNT }
  | "MEAN" | "mean" { MEAN }
  | "SD" | "sd" { SD }
  | "s" | "sec" | "seconds" { SECONDS }
  | "m" | "min" | "minutes" { MINUTES }
  | "h" | "hr" | "hours" { HOURS }
  | "d" | "days" { DAYS }
  | "TIMESTAMP" | "timestamp" { TIMESTAMP }
  | "TAG" | "tag" { TAG }
  | "VALUE" | "value" { VALUE }
  | ','      { COMMA }
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | '['      { LEFT_BRACK }
  | ']'      { RIGHT_BRACK }
  | '='      { EQUALS }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }


and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

