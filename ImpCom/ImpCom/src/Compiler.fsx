// Learn more about F# at http://fsharp.org

open Result
open Iter
open Mapping
open Regex
open DFA


[<EntryPoint>]
let main argv =
    let test = List("(a|b)*abb")
    (*
        0: (b|aa) paranteses
        1: b|aa orr
        2: | b a :: a atom
        3: | b a a 
    *)
    
    let table, size, min' =
        Run Tokenizer test
        |> Return 
        |> fun (tokens, _) -> 
            lst(tokens)
        |> Run Parser
        |> Return
        |> fun ((regex, count), _) ->  
           Cat regex (Terminal count) 
        |> StateFinder
        |> fun (language, states, transitions) ->

            let t = 
                Map.toList transitions
                |> List.map (fun ((src,letter),dst) -> ((src, char letter), dst))
                |> Map.ofList
            makeTable language states transitions 
   
    DfaMap table size min' [|None;None;None;Some id|]
    |> Run
    |> fun func -> func (List(argv.[0]))
    |> fun ret ->
        match ret with
        | Failure msg -> printfn "ERROR: %s" msg
        | Success(ret, _) -> printfn "SUCCESS: %s" ret
    
    0 // return an integer exit code


    (*
                 a  b
        state 1: 2  1
        state 2: 2  3
        state 3: 2  4
        state 4: 2  1

    *)