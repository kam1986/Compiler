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
[<Struct>]
type 'Type Token =
    val tp : 'Type
    val value : token
    val pos : Position
    new(tp,data,pos) = { tp=tp; value=data; pos=pos}


let inline TypeOf (token : _ Token) = token.tp

let inline ValueOf (token : _ Token) = 
    let t = Take token.value
    id t 

let inline PosOf (token : _ Token) = token.pos
  
