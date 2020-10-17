module Typing

(*
    This is a lib for types in the imperative language used by the type checker
*)
type PRIMITIVE = INT | FLOAT | CHAR | BOOL | POINTER
type TYPE = 
    | Primitive of PRIMITIVE
    | ARRAY     of TYPE list
    | STRUCT    of name : string * (string * TYPE) list
    | UNION     of name : string * (string * TYPE) list
    | FUNC      of name : TYPE list * TYPE list 