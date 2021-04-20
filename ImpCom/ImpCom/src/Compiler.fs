// Learn more about F# at http://fsharp.org
// fix mapping in table construction of LEXER
// fix placing of actions in the action table
open Return
open Iter

open Token
open Lexer
open Parser
open Patterns
open SymbolTable




[<EntryPoint>]
let main _ =
    let content = FromString " (0 + 3) mod 2"
    Tokens lexer content                               // return a sequence of tokens from the language of the 'lexer' as put
    |> Seq.filter (fun token -> TypeOf token <> NOISE) // user defined filter of tokens
    |> Run parser                                      // parse token sequence
    |> printfn "result %A"

    0 // return an integer exit code


