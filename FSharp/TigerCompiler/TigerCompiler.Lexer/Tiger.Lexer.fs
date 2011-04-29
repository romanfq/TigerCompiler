{
open System
open Tiger.Core
open Lexing

let inc_lnum bol pos = 
  let lnum = pos.pos_lnum in 
  {pos with pos_lnum =  lnum+1; pos_bol = bol }
 
 
 
let newline lexbuf = 
  lexbuf_set_curr_p lexbuf 
    ( inc_lnum (lexeme_end lexbuf) (lexeme_end_p lexbuf))
}

let digit=['0'-'9']
let whitespace = [' ', '\t']
let newLine = ('\n' | '\r' '\n')

rule parsetokens = parse
| whitespace  { parsetokens lexbuf } // ignore whitespace
| newLine     { newline lexbuf; parsetokens lexbuf }
| ['-']? digit+ { INT(System.Int32.Parse(lexeme lexbuf) }
  


