module AST

(*

    This is the abstract syntax tree for a very simple imperative language
    We only added one of each base primitive type, but are easily extensible 

    For the moment being there is no ability to create functions, but this is planned to be added


    A correctly parsed program under the gramma given this language will be a tuple of a (AST, value symboltable, Function symboltable, type table)
    This will be done by the actions made by the parser. To disconnect the type declaration from the syntax tree makes some of the code alot smaller.
    It will have the side effect that all types will be global to the file indifferent of the position in the file.

*)

// intentionally splitted between simple and none simple types of simpler code
type Primitive =
    | Int       of int32 
    | Float     of float 
    | Char      of int32  // enable most text encoding schemes 
    | Bool      of bool 
    | Pointer   of int64 

// it is importent to notice that the defining of a new struct type are not given as part of the syntax tree 
// this should be handled by the parsing fase
type Type = 
    | Primitive of Primitive
    | Array     of size : int64 * array : Type list // strings should be of this type too
    | Struct    of name : string * fields : (string * Type ) list
    | Union     of name : string * fields : (string * Type ) list
    | Tuple     of Type list 


type Expression =
    | Constant      of Type
    | Variable      of ID : string
    | Add           of Expression * Expression
    | Sub           of Expression * Expression
    | Mul           of Expression * Expression
    | Div           of Expression * Expression
    | LeftShift     of Expression * Expression
    | RightShift    of Expression * Expression
    | Match         of item : Expression * cases : (Expression * Expression) list


type Condition =
    | Truth             of Type
    | Variable          of name : string
    | Not               of Condition 
    | And               of Condition * Condition
    | Or                of Condition * Condition
    | Xor               of Condition * Condition
    | Imply             of Condition * Condition // just for fun, often feel like I'm missing this fellow
    | Equal             of Expression * Expression
    | NotEqual          of Expression * Expression
    | Less              of Expression * Expression
    | Greater           of Expression * Expression
    | LessOrEqual       of Expression * Expression
    | GreaterOrEqual    of Expression * Expression


// should possibly be added more to this
type Statement =
    | Assign        of name : string * value : Expression
    | AssignEntry   of name : string * index : Expression * value : Expression 
    | Sequence      of Statement * Statement
    | IfThen        of Condition * Statement
    | IfThenElse    of Condition * Statement * Statement
    | While         of Condition * Statement
    | DoWhile       of Condition * Statement



