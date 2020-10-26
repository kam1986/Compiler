// Learn more about F# at http://fsharp.org

open Result
open Iter
open Mapping
open Regex
open DFA
open Lexer

type Token = 
    | LET 
    | ID of string 
    | VALUE of int 
    | IF 
    | THEN 
    | ELSE 
    | EQ 
    | LPAR | RPAR
    | SPACE 
    | BOOL of bool


[<EntryPoint>]
let main argv =
    
    let lexer = 
        Lexer(
            [|
                "let",                  fun _ -> LET
                "if",                   fun _ -> IF
                "then",                 fun _ -> THEN
                "else",                 fun _ -> ELSE
                "=",                    fun _ -> EQ
                " ",                    fun _ -> SPACE
                "\\(",                  fun _ -> LPAR // escaped symbol syntax
                "\\)",                  fun _ -> RPAR
                "true",                 fun _ -> BOOL true
                "false",                fun _ -> BOOL false
                "[0-9]*",               (VALUE << int)
                "[A-z][a-z0-9]*",      ID // added last because of token precedence
            |]
        )

   

    let ret = 
        LexAll lexer (List("let Atest = 1234 if true then  ( true ) else false"))
        |> map (List.filter (fun token -> token <> SPACE))
    
    printfn "%A" ret

    
    0 // return an integer exit code

