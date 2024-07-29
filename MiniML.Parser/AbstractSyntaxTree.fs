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
    | List of Expression list
    | Tuple of Expression list
    | Symbol of string
    | Operator of Operator * Expression * Expression
    | Invoke of string * Expression list
    | Function of Binding * Statements
    | IfThenElse of Expression * Statements * Statements
    | Match of Expression * (Expression * Statement list) list

and [<RequireQualifiedAccess>] Statement =
    | Return of Expression
    | Let of Binding * Statements
    | While of Expression * Statement list
    | For of Binding * Expression * Statement list

and Statements = Statement list

[<RequireQualifiedAccess>]
type File =
    | Module of string * Statements
