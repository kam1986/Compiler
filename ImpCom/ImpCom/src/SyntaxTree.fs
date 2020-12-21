module SyntaxTree

open Position



type Token = 
    // value tokens
    | INT
    | FLOAT
    | BOOL
    | BYTE
    | STRING
    | ID

    // aritmetic tokens
    | PLUS
    | MINUS
    | TIMES
    | DIVIDE
    | EQUAL
    | LESS
    | GREATER
    | MODULUS


    // keywords
    | LET
    | MUT
    | CON
    | SPACE
    | WHEN
    | IS
    | EQ 
    | LPAR | RPAR
 
type NonTerminal =
    | Exp
    | Val
    
// should be expanded to cover more value type later
type value =
    | Int64 of int * Position
    | Float of float * Position
    | Bool of byte * Position
    | Str of string * Position
    | Nothing

type 'Type Expression =
    // some value
    | Value     of value * Position
    // variable name
    | Variable  of string * Position
    // arithmetic 'Type Expression
    | Not       of 'Type * ('Type Expression * Position)
    | Neg       of 'Type * ('Type Expression * Position)
    | Add       of 'Type * ('Type Expression * 'Type Expression * Position)
    | Sub       of 'Type * ('Type Expression * 'Type Expression * Position)
    | Mul       of 'Type * ('Type Expression * 'Type Expression * Position)
    | Div       of 'Type * ('Type Expression * 'Type Expression * Position)
    | Mod       of 'Type * ('Type Expression * 'Type Expression * Position)
    | And       of 'Type * ('Type Expression * 'Type Expression * Position)
    | Or        of 'Type * ('Type Expression * 'Type Expression * Position)
    | Xor       of 'Type * ('Type Expression * 'Type Expression * Position)
    | Imply     of 'Type * ('Type Expression * 'Type Expression * Position)
    | LSwift    of 'Type * ('Type Expression * 'Type Expression * Position)
    | RSwift    of 'Type * ('Type Expression * 'Type Expression * Position)
    (* multi ary branching form 
            when item is 
                i1 -> ret1
                i2 -> ret2
                ..
                in -> retn
    *)
    // input and output type should be identical for all cases
    | WhenIs    of ('Type * 'Type) * Item : 'Type Expression * Cases : ('Type Expression * 'Type Expression) list * Position
    // defining variable, this should not contain type information since the expression given are typed and on
    // lookup we have access to type information.
    | LetIn     of string * 'Type Expression * 'Type Expression
    | Func      of InOutTypes : ('Type list * 'Type) * Name : string * Args : 'Type Expression list * Body : 'Type Expression 