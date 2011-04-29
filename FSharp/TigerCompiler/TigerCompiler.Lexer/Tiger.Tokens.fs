// The definition of the lexical rules of the Tiger Language. 
// See http://www.lrde.epita.fr/~akim/ccmp/tiger.html#Tiger-Language-Reference-Manua

module Tiger.Core

type Tokens = 
| INT of int
| ARRAY 
| IF 
| THEN
| ELSE
| WHILE
| FOR 
| TO
| DO
| LET
| IN
| END
| OF
| BREAK
| NIL
| FUNCTION
| VAR
| TYPE
| IMPORT
| PRIMITIVE