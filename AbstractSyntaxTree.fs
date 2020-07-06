module AbstractSyntaxTree
 
type Function =
    | Plus
    | Minus
    | Times
    | Divide

type Expression = 
    | Number of decimal
    | Symbol of string
    | Function of Function * Expression * Expression

type Statement =
    | Return of Expression
    | Let of string * Statements
and Statements = Statement list

type File =
    | Module of string * Statements

