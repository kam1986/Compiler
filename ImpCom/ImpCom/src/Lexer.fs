module Lexer

#nowarn "64"

open Result
open Mapping
open Iter
open Regex
open DFA

let LexError msg =
    sprintf "Lexing Error:\n\t%s" msg
    |> Failure

let private Atom expect =
    let inline a input =
        match Next (input : 'input Iter) with
        | Failure msg -> 
            Prev input
            Failure msg
        | Success(b, next) when expect = b -> Success(b, next)
        | Success(b, next) ->
            Prev next
            sprintf "Expected byte %d but got byte %d" expect b
            |> LexError
    Map a

// TODO Need to rework this to handle regular expression
let private lexeme pattern token =
    fun input ->
        match Run pattern input with
        | Success(_, iter) -> Success(token, iter)
        | Failure msg -> Failure msg
    |> Map



[<Struct>]
type Lexer<'token> =
    val private pattern : Map<byte, 'token>
    private new (pattern) = { pattern = pattern }
    new (tokens : array<string * (string -> 'token)>) =
        assert(tokens.Length > 0)
        let mutable count = 0
        let mutable term = 0
        let patterns, accepts = Array.unzip tokens
        let regex = // taking each pattern and making a big regex
            Array.map (
                fun pattern -> 
                    Run Tokenizer (List(pattern))
                    |> debugReturn
                    |> fun (tokens, _) -> lst(tokens)
                    |> Run (Parser count)
                    |> debugReturn
                    |> fun ((regex, count'),_) ->
                            count <- count'
                            term <- term - 1
                            Cat regex (Terminal term)
            ) patterns
            |> Array.reduce Or

        let (_, states, _) as ret = StateFinder regex
        let table, size', min' = makeTable ret
        let maybeAccept = getAcceptancePrState states accepts
        let map = DfaMap table size' min' maybeAccept

        { pattern = map }

    static member LexNext (pattern : Lexer<'token>) = Run pattern.pattern 
    
    

let LexNext (pattern : Lexer<'token>) = Lexer.LexNext pattern

let LexAll pattern =
    let rec acc tokens input =
        match LexNext pattern input with
        | Success (token, iter) -> acc (token :: tokens) iter
        | Failure _ -> 
            match IsEmpty input with
            | true -> Success(List.rev tokens)
            | _ -> Failure "Lexical error"
    acc []
   
