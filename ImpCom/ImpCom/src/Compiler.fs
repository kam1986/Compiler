// Learn more about F# at http://fsharp.org

open Result
open Iter
open Mapping
open Regex
open DFA
open Lexer

type Token = LET | ID of string | VALUE of int | THEN | ELSE | IF | EQ | SPACE
[<EntryPoint>]
let main argv =
    
    let lexer = 
        Lexer(
            [|
                "let",                  fun _ -> LET
                "if",                   fun _ -> IF
                "[A-Z][a-z0-9A-Z]*",    ID 
                " ",                    fun _ -> SPACE
                "then",                 fun _ -> THEN
                "else",                 fun _ -> ELSE
                "=",                    fun _ -> EQ
                "[0-9]*",               (VALUE << int)
            |]
        )

    let rets = 
        List.map(fun str -> LexAll lexer (List(str))) ["let Atest = 1234 if else then"]
        |> List.map (map (List.filter (fun token -> token <> SPACE)))
    for ret in rets do
        printfn "%A" ret

    0 // return an integer exit code


    (*
                 a  b
        state 1: 2  1
        state 2: 2  3
        state 3: 2  4
        state 4: 2  1

    *)