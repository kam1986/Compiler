module Lexer

(*

    TODO: 
       - change the conversion of the token to value to result type where we get som error formatter for the specifec case
       - Change token type and postpond the conversion of value undtil later
       - Change the iter to Some interface type that support read + seek + next
*)

#nowarn "25" "64"

open Return
open Mapping
open Iter
open Regex
open DFA
open TypeAcrobatics
open Token

let LexError msg =
    sprintf "Lexing Error:\n\t%s" msg
    |> Error


let ( != ) (str : string) (token : 't when 't : equality, ret) = (str, (token, fun input -> Delay ret input))
let ( := ) (str : string) (token : 't when 't : equality) = (str, (token, fun _ -> Arg null)) // should not be used to anything
let ( --> ) ret (token : 't when 't : equality) = (token, ret)


[<Struct>]
type Lexer<'token when 'token : equality> =
    val private pattern : Map<byte, 'token Token>
    private new (pattern) = { pattern = pattern }
    new (tokens : array<string * ('token * (string -> token))>) =
        assert(tokens.Length > 0)
        let mutable count = 0
        let mutable term = 0
        let patterns, accepts = Array.unzip tokens
        let regex = // taking each pattern and making a big regex
            Array.map (
                fun pattern -> 
                    Run Tokenizer (FromString(pattern))
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

    static member LexNext (pattern : Lexer<'a>) = Run pattern.pattern 
    
    

let internal LexNext pattern = Lexer<'a>.LexNext pattern


let FindAll pattern input =
    let rec sequence input =
           match LexNext pattern input with
           | Ok (token, iter) -> 
               seq { 
                   yield token
                   yield! sequence iter
               }
           | Error _ -> 
               match IsEmpty input with
               | true -> Seq.empty
               | _ ->
                   // if no match skip one byte and try again until end of sequence
                   // loop invariance is that we know that the sequence contains atleast one more byte
                   // nowarn "25" course the compiler to ignore incomplete pattern matching here it is needed because we know it is true but the compiler can not figure it out
                   let (Ok(_, iter)) = Next input
                   sequence iter
    sequence input


let Tokens pattern input =
    let rec sequence input =
        match LexNext pattern input with
        | Ok (token, iter) -> 
            seq { 
                yield token
                yield! sequence iter
            }
        | Error msg -> 
            match IsEmpty input with
            | true -> Seq.empty
            | _ ->
                printfn "%s" msg
                exit -1
    
    sequence input
