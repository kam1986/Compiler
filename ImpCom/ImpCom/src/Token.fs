module Token

open Position
open TypeAcrobatics
(*
    This is a generic token type, used both in lexing and the parsing fase
    the integer is meant to enhance pattern matching by converting it to a DFA
    
    delay is a delayed transformation it simply delay the transformation of the string until we put a specific value into the parse tree

    this makes it possible to both pattern match specific tokens and get a generic type for any type feed into the lexer and parser

    ID might be used to track last state in the parser DFA
*)

type 'Type Token = Token of 'Type * token * Position

let inline TypeOf (Token(tp, _, _)) = tp

let inline ValueOf (Token(_, value, _)) = Take value

let inline PosOf (Token(_, _, pos)) = pos
  
