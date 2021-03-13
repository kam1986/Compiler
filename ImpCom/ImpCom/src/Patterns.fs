module Patterns

open Token
open Lexer
open Productions
open Parser

// This is the lexing pattern for the compiler
type tok = 
    | INT  | FLOAT // | ID    | STR 
    | PLUS | MINUS | TIMES | DIVIDE 
    | LPAR | RPAR  | NOISE  // | LSQR | RSQR | LBRA | RBRA

let lexer =
    [|
        "\+"                                := PLUS
        "\-"                                := MINUS
        "/"                                 := DIVIDE
        "\*"                                := TIMES
        "\("                                := LPAR
        "\)"                                := RPAR
       (* 
        "\["                                := LSQR
        "\]"                                := RSQR
        "\{"                                := LBRA
        "\}"                                := RBRA
        *)
        
        "[0-9]+"                            != int      --> INT
        "([0-9]+.[0-9]*)|([0-9]*.[0-9]+)"   != float    --> FLOAT
        // "[A-Z][A-Za-z0-9_]*"                != string   --> ID
        //"\"([^\"]|(\\\"))*\""               != string   --> STR // test it

        // noise
        " *"                                := NOISE
        "\n"                                := NOISE
        "\r"                                := NOISE
        "\t"                                := NOISE
    |]
    |> Lexer


type Language = Exp | Func | Val
// this is the parsing pattern for the compiler
type e = Exp | Exp' | Val

// OBS! if for some reason you havn't implemented all Tokens into the productions, it will course an error
let parser =
    Productions [
        Exp => [
            [%Exp; !PLUS; %Exp'] 
            >> fun args -> ValueOf args.[0] + ValueOf args.[2]

            [%Exp; !MINUS; %Exp'] 
            >> fun args -> ValueOf args.[0] - ValueOf args.[2]

            [%Exp']
            >> fun args -> ValueOf args.[0]
        ]

        Exp' => [
            [%Exp'; !TIMES; %Val] 
            >> fun args -> ValueOf args.[0] * ValueOf args.[2]

            [%Exp'; !DIVIDE; %Val] 
            >> fun args -> ValueOf args.[0] / ValueOf args.[2]
            
            [%Val]
            >> fun args -> ValueOf args.[0]
        ]

        Val => [
            [!LPAR; %Exp; !RPAR]
            >> fun args -> ValueOf args.[1]

            [!INT]
            >> fun args -> ValueOf args.[0]

            [!FLOAT]
            >> fun args -> ValueOf args.[0]
            
        ]
    ]
    |> SLR