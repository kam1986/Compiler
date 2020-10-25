module Result


// could have used the Result<'ok,'error> from F# but no
[<Struct>]
type 'ret Result =
    | Failure of err : string
    | Success of ret :'ret

let debugReturn ret =
    let test ret =
        match ret with
        | Success _ -> true
        | Failure msg -> 
            printfn "%s" msg
            false
    assert test ret
    match ret with
    | Success ret -> ret
    | Failure msg ->
        printfn "%s" msg
        exit -1

let Return ret =
    match ret with
    | Success ret -> ret
    | Failure msg ->
        printfn "%s" msg
        exit -1