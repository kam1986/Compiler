module Lexer

open Result
open Mapping
open Iter
open System.Text


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
    new (tokens : seq<string * 'token>) =
        let pattern = 
            // decompose to byte representation
            Seq.map (fun (expect, ret) -> (Encoding.Default.GetBytes(expect: string), ret)) tokens
            |> Seq.map (fun (bytes : byte[], ret) ->
                if bytes.Length = 0 then 
                    (&[], ret)
                else 
                    // makes map for each byte array
                    (Array.map (fun byte -> Atom byte) (bytes : byte[]) 
                    |> fun patterns -> Reduce (fun x xs -> x :: xs) [] patterns, ret)
                    )
            // make a map that tries every combinations og strings as bytes and return the token
            |> Seq.map (fun (expect, token) -> lexeme expect token) // TODO alter here to passe acculator to token
            |> Seq.reduce ( <|> )
        Lexer(pattern)
       
    
    static member Lex (pattern : Lexer<'token>) = Run pattern.pattern

let Lex pattern = Lexer.Lex pattern

type token = CAKE | TAKE
let test = Lexer([("cake", CAKE); "take", TAKE])

Lex test (List("bake") :> byte Iter) |> ignore
