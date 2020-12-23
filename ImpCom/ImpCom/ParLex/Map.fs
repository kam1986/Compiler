module internal Mapping

open Return
open Iter

// This is a abstraction of some function which can fail
// are using Struct to better hide the internal structure
[<Struct>]
type Map<'input, 'output> =
    val private pattern : 'input Iter -> Result<'output * ('input Iter), string>
    new pattern = { pattern = pattern } // constructor

    static member Run (map : Map<'intput,'output>) = map.pattern


let Run map input = Map.Run map input


let ( ~& ) item = 
    fun input -> Ok(item, input)
    |> Map


// either map1 or map2
let ( <|> ) map1 map2 =
    fun input ->
        match Run map1 input with
        | Ok output -> Ok output
        | Error _ ->
            match Run map2 input with
            | Ok output -> Ok output
            | Error msg -> Error msg
    |> Map


// map1 and map2
let ( <&> ) map1 map2 =
    fun input ->
        match Run map1 input with
        | Error msg -> Error msg
        | Ok (output1, rest) ->
            match Run map2 rest with
            | Error msg -> Error msg
            | Ok(output2, rest) -> Ok((output1, output2), rest)
    |> Map

// map map with func
let ( >> ) func map =
    fun input ->
        match Run map input with
        | Error msg -> Error msg
        | Ok (output, rest) -> Ok(func output, rest)
    |> Map


let ( => ) func param1  =
    func <&> param1
    |> (>>) (fun (func, param) -> func param)

let Lift2 func param1 param2 =
    &func => param1 => param2


let Reduce serializer acc mlst =
    Seq.foldBack (fun acc next -> (Lift2 serializer) acc next) mlst &acc

let ErrorMap f map =
    fun input ->
        match Run map input with
        | Ok ret -> Ok ret
        | Error msg -> Error (f msg)


