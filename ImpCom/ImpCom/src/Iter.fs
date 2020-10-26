module Iter

open System.IO
open Result
open Decoding


type 'I Iter =
    abstract member Next : Result<'I * 'I Iter>
    abstract member Prev : unit
    abstract member Show : unit
    abstract member IsEmpty : bool


let Next (iter : 'I Iter) = iter.Next
let Prev (iter : 'I Iter) = iter.Prev
let Show (iter : 'I Iter) = iter.Show
let IsEmpty (iter: 'I Iter) = iter.IsEmpty


[<Struct>]
type List =
    val private buf : byte list
    private new(xs) = {buf = xs}
    new(str : string) = { buf = GetBtyes str } 

    static member FromFile(path) =
        use file = System.IO.File.OpenText(path)
        List(file.ReadToEnd())

    interface byte Iter with
        member L.Next =
            match L.buf with
            | []      -> Failure "End of Stream"
            | x :: xs -> Success(x, List(xs) :> byte Iter)
        
        member L.Prev = ()

        member L.Show = printfn "%A" L.buf

        member L.IsEmpty = List.isEmpty L.buf



// meant for network streams
[<Struct>]
type StreamIter =
    val private buf : Stream
    new(stream) = { buf = stream }
    
    interface byte Iter with
        member B.Next =
            try
                Success(byte <| B.buf.ReadByte(), B :> byte Iter)
            with
            | error -> Failure error.Message 

        member B.Prev = ()

        member B.Show = printfn "A stream"

        member B.IsEmpty = false



[<Struct>]
type 'I lst =
    val private buf : 'I list
        new(cs) = { buf = cs }
    interface 'I Iter with
        member L.Next =
            match L.buf with
            | []      -> Failure "End of Stream"
            | x :: xs -> Success(x, lst(xs) :> 'I Iter)
    
        member L.Prev = ()

        member L.Show = printfn "%A" L.buf

        member L.IsEmpty = List.isEmpty L.buf