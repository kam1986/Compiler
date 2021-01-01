// Learn more about F# at http://fsharp.org
// fix mapping in table construction of LEXER
// fix placing of actions in the action table
open Return
open Iter
open Lexer
open Token
open Productions
open Parser
open SymbolTable




type e = Exp 
type tok = INT | PLUS | TIMES | NOISE

let lexer =
    [|
        "\+"        := PLUS
        "\*"        := TIMES
        " *"        := NOISE
        "[0-9]+"    != INT --> int
    |]
    |> Lexer


let parser =
    Productions [
        Exp => [
            [%Exp; !PLUS; %Exp] 
            >> fun args -> ValueOf args.[0] + ValueOf args.[2]
            
            [!INT]
            >> fun args -> ValueOf args.[0]
        ]
    ]
    |> SLR




[<EntryPoint>]
let main _ =
    let content = FromString("1 + 1 + 2")
    Tokens lexer content                                // return a sequence of tokens from the langue 'lexer' in input
    |> Seq.filter (fun token -> TypeOf token <> NOISE)
    |> parser.Parse                                     // remove space tokens before parsing
    |> printfn "%A"

    0 // return an integer exit code


