module MiniML.AbstractSyntaxTree
 
[<RequireQualifiedAccess>]
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

[<RequireQualifiedAccess>]
type Expression = 
    | Number of decimal
    | Symbol of string
    | Function of Function * Expression * Expression
    | FunctionCall of string * Expression list
    | PartialFunctionCall of string * (Expression list)

[<RequireQualifiedAccess>]
type Binding =
    | Symbol of string

[<RequireQualifiedAccess>]
type Statement =
    | Return of Expression
    | Let of Binding * Statements
    | IfThenElse of Expression * Statements * Statements
    | Match of Expression * (Expression * Statement list) list
    | While of Expression * Statement list
    | For of Binding * Expression * Statement list
and Statements = Statement list

[<RequireQualifiedAccess>]
type File =
    | Module of string * Statements
