// Learn more about F# at http://fsharp.org
// fix mapping in table construction of LEXER
// fix placing of actions in the action table
open Return
open Iter
open Mapping
open Regex
open DFA
open Lexer
open TypeAcrobatics
open Token
open Productions
open Parser
open Position
open SyntaxTree


type NT = T | R
type Term = A | B | C
(*
let TP = 
    Productions [
        T => [
            [&R] >> fun args -> ValueOf args.[0]
            [!A; &T; !C] >> fun args -> string (ValueOf args.[0]) + string (ValueOf args.[1]) + string (ValueOf args.[2])
        ]
        R => [
            [] >> fun _ -> ()
            [!B;&R] >> fun args -> string (ValueOf args.[0]) + string (ValueOf args.[1])
        ]
    ]
    |> SLR.Create

let TL =
    [|
        "a" != A --> string
        "b" != B --> string
        "c" != C --> string
    |]
    |> Lexer
*)



type e = Exp
type tok = INT | PLUS |  LPAR | RPAR | NOISE

let lexer =
    [|
        "[0-9]+" != INT --> int
        "\+" := PLUS
        "\(" := LPAR
        "\)" := RPAR
        " *" := NOISE
    |]
    |> Lexer

let parser =
    Productions [
        Exp => [
            [!INT; !PLUS; &Exp] 
            >> fun args -> ValueOf args.[0] + ValueOf args.[2]

            [!LPAR; &Exp; !RPAR; !PLUS; &Exp]
            >> fun args -> ValueOf args.[1] + ValueOf args.[4]

            [!LPAR; &Exp; !RPAR]
            >> fun args -> ValueOf args.[1]
            
            [!INT]
            >> fun args -> ValueOf args.[0]
        ]
    ]
    |> SLR.Create

[<EntryPoint>]
let main _ =
  
    
    
    let test = FromFile("C:\Users\KAM\OneDrive\Skrivebord/test.txt")
    Tokens lexer test                                   // return a sequence of tokens from the langue 'lexer' in input
    |> Seq.filter (fun token -> TypeOf token <> NOISE)
    |> parser.Parse                                     // remove space tokens before parsing
    |> printfn "%A"

    0 // return an integer exit code


