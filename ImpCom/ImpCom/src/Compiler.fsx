// Learn more about F# at http://fsharp.org

open Result
open Iter
open Mapping
open Regex
open DFA


[<EntryPoint>]
let main argv =
    let test = List("[a-b]+[1-9].")
    (test :> byte Iter).Show
    (*
        0: (b|aa) paranteses
        1: b|aa orr
        2: | b a :: a atom
        3: | b a a 
    *)
    
    let table, _ =
        Run Tokenizer test
        |> Return 
        |> fun (tokens, _) -> 
            printfn "tokens\n\t%A" tokens
            printfn ""
            lst(tokens)
        |> Run Parser
        |> Return
        |> fun ((regex, count), _) ->  
            printfn "Regex\n\t%A" (Cat regex (Terminal count))
            printfn "FollowPos\n\t%A" <| Followpos (Cat regex (Terminal count)) Map.empty
            Cat regex (Terminal count) 
        |> StateFinder
        |> fun (language, states, transitions) ->
            printfn "language\n%A\n" language
            let t = 
                Map.toList transitions
                |> List.map (fun ((src,letter),dst) -> ((src, char letter), dst))
                |> Map.ofList

            printfn "transitions\n%A\n" t
            printfn "states\n%A\n" states
            makeTable language states transitions

    printfn "%A" table

    0 // return an integer exit code


    (*
                 a  b
        state 1: 2  1
        state 2: 2  3
        state 3: 2  4
        state 4: 2  1

    *)