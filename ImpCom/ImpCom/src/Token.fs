module Token
open TypeAcrobatics

[<Struct>]
type Position = 
    { 
        Line : int 
        Offset : int 
        Indentation : int 
        Absolut : int
    }

let Start = 
    { 
        Line = 0
        Offset = 0
        Indentation = 0
        Absolut = 0
    }

let Move pos steps = 
    let offset = pos.Offset + steps
    { pos with 
        Offset = offset 
        Absolut = pos.Absolut + steps
        Indentation = offset / 4 // set indentation to enhance performance on indentation grammar
    }

let Next pos = Move pos 1

let Newline pos = 
    { 
        Absolut = pos.Absolut + 1
        Line = pos.Line + 1
        Offset = 0
        Indentation = 0
    }


(*
    This is a generic token type, used both in lexing and the parsing fase
    the integer is meant to enhance pattern matching by converting it to a DFA
    
    delay is a delayed transformation it simply delay the transformation of the string until we put a specific value into the parse tree

    this makes it possible to both pattern match specific tokens and get a generic type for any type feed into the lexer and parser

    ID might be used to track last state in the parser DFA
*)
[<Struct>]
type Token = Token of  ID : int * Value : Arg * Pos : Position

let ValueOf (Token (_, v, _)) = Take v
let PosOf (Token (_, _, p)) = p