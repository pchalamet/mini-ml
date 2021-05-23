module AbstractSyntaxTree
 
type Function =
    | Plus
    | Minus
    | Times
    | Divide
    | Equal
    | NotEqual
    | LowerThan
    | LowerThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | And
    | Or
    | Not

type Expression = 
    | Number of decimal
    | Symbol of string
    | Function of Function * Expression * Expression

type Statement =
    | Return of Expression
    | Let of string * Statements
    | IfThenElse of Expression * Statements * Statements
and Statements = Statement list

type File =
    | Module of string * Statements

