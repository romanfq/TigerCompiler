namespace Ast
open System

type Id = 
    | Identifier of string

and LValue =
    | LValueId        of Id
    | MemberAccess    of LValue * Id
    | ArrayAccess     of LValue * Expr

and Value = 
    | Integer   of Int32
    | Str       of string
    | Nil
    | Id        of Id
    | Negative  of Expr
    | ParenExp  of Expr
    | LValue    of LValue

and Factor = 
    | Value         of Value
    | NegativeValue of Value

and Term = 
    | Factor    of Factor 
    | Multiply  of Factor * Term   
    | Divide    of Factor * Term

and FieldAssignment = 
    | FieldAssignment of Id * Expr

and Declaration = 
    | TypeDeclaration   of Id * TypeDef
    | VarDeclaration    of Id * (Id option) * Expr
    | FuncDeclaration   of Id * (TypeField list option) * (Id option) * Expr

and TypeDef = 
    | TypeIdDef of Id
    | TypeFieldsDef of (TypeField list option)
    | ArrayTypeDef of Id
   
and TypeField = 
    | TypeField of Id * Id 
      
and Expr =
    | Plus of Term * Expr
    | Minus of Term * Expr
    | And of Term * Expr
    | Or of Term * Expr
    | Equals of Term * Expr
    | NotEquals of Term * Expr
    | GreaterThan of Term * Expr
    | GreaterThanEqual of Term * Expr
    | LessThan of Term * Expr
    | LessThanEqual of Term * Expr 
    | Term of Term
    | Array of Id * Expr * Expr
    | Record of Id * (FieldAssignment list)
    | Assign of LValue * Expr
    | FunctionCall of Id * (Expr list)
    | ExprSeq of (Expr list)
    | Conditional of Expr * Expr * (Expr option)
    | While of Expr * Expr
    | For of Id * Expr * Expr * Expr
    | Break
    | Let of (Declaration list) * (Expr list)

and Program = 
    | Program of Expr  