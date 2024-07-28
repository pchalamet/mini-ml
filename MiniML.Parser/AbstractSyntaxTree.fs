module MiniML.AbstractSyntaxTree
 
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
    | FunctionCall of string * Expression list
    | PartialFunctionCall of string * (Expression list)

type Statement =
    | Return of Expression
    | Let of string * Statements
    | IfThenElse of Expression * Statements * Statements
    | Match of Expression * (Expression * Statement list) list
    | While of Expression * Statement list
and Statements = Statement list

type File =
    | Module of string * Statements

