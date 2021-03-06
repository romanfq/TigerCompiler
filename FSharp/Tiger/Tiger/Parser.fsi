// Signature file for parser generated by fsyacc
module Parser
type token = 
  | EOF
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
  | LPAREN
  | RPAREN
  | LCURLY
  | RCURLY
  | LBRACKET
  | RBRACKET
  | COMMA
  | SEMICOLON
  | COLON
  | QUOTE
  | ASSIGN
  | DOT
  | PLUS
  | MINUS
  | ASTER
  | SLASH
  | EQ
  | NOTEQ
  | LT
  | LTE
  | GTE
  | GT
  | AND
  | OR
  | ID of (System.String)
  | STRING of (System.String)
  | INT of (System.Int32)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_ARRAY
    | TOKEN_IF
    | TOKEN_THEN
    | TOKEN_ELSE
    | TOKEN_WHILE
    | TOKEN_FOR
    | TOKEN_TO
    | TOKEN_DO
    | TOKEN_LET
    | TOKEN_IN
    | TOKEN_END
    | TOKEN_OF
    | TOKEN_BREAK
    | TOKEN_NIL
    | TOKEN_FUNCTION
    | TOKEN_VAR
    | TOKEN_TYPE
    | TOKEN_IMPORT
    | TOKEN_PRIMITIVE
    | TOKEN_LPAREN
    | TOKEN_RPAREN
    | TOKEN_LCURLY
    | TOKEN_RCURLY
    | TOKEN_LBRACKET
    | TOKEN_RBRACKET
    | TOKEN_COMMA
    | TOKEN_SEMICOLON
    | TOKEN_COLON
    | TOKEN_QUOTE
    | TOKEN_ASSIGN
    | TOKEN_DOT
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_ASTER
    | TOKEN_SLASH
    | TOKEN_EQ
    | TOKEN_NOTEQ
    | TOKEN_LT
    | TOKEN_LTE
    | TOKEN_GTE
    | TOKEN_GT
    | TOKEN_AND
    | TOKEN_OR
    | TOKEN_ID
    | TOKEN_STRING
    | TOKEN_INT
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_Program
    | NONTERM_Expr
    | NONTERM_Term
    | NONTERM_Factor
    | NONTERM_Value
    | NONTERM_ArrayDecl
    | NONTERM_RecordDecl
    | NONTERM_FieldList
    | NONTERM_FieldAssignment
    | NONTERM_Assignment
    | NONTERM_LValue
    | NONTERM_FunctionCall
    | NONTERM_ExprList
    | NONTERM_ExprSequence
    | NONTERM_Conditional
    | NONTERM_Loop
    | NONTERM_LetDecl
    | NONTERM_DeclList
    | NONTERM_Decl
    | NONTERM_TypeDecl
    | NONTERM_TypeDef
    | NONTERM_TypeFields
    | NONTERM_TypeField
    | NONTERM_VarDecl
    | NONTERM_FuncDecl
    | NONTERM_Id
/// This function maps integers indexes to symbolic token ids
val tagOfToken: token -> int

/// This function maps integers indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Program) 
