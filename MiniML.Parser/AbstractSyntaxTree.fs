module MiniML.AbstractSyntaxTree
 
[<RequireQualifiedAccess>]
type Operator =
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
type Binding =
    | Symbol of string

[<RequireQualifiedAccess>]
type Expression = 
    | Number of decimal
    | Symbol of string
    | Operator of Operator * Expression * Expression
    | Invoke of string * Expression list
    | Function of Binding * Statements
and
    [<RequireQualifiedAccess>] 
    Statement =
    | Return of Expression
    | Let of Binding * Statements
    | IfThenElse of Expression * Statements * Statements
    | Match of Expression * (Expression * Statement list) list
    | While of Expression * Statement list
    | For of Binding * Expression * Statement list
and
    Statements = Statement list

[<RequireQualifiedAccess>]
type File =
    | Module of string * Statements
