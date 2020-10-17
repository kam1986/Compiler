module Iter

#r "../bin/Result.dll"
#r "../bin/Decoding.dll"
open Result
open Decoding


type 'I Iter =
    abstract member Next : Result<'I * 'I Iter>
    abstract member Prev : unit
    abstract member Show : unit


let Next (iter : 'I Iter) = iter.Next
let Prev (iter : 'I Iter) = iter.Prev
let Show (iter : 'I Iter) = iter.Show


[<Struct>]
type List =
    val private buf : byte list
    private new(xs) = {buf = xs}
    new(str : string) = { buf = GetBtyes str } 
    interface byte Iter with
        member L.Next =
            match L.buf with
            | []      -> Failure "End of Stream"
            | x :: xs -> Success(x, List(xs) :> byte Iter)
        
        member L.Prev = ()

        member L.Show = printfn "%A" L.buf

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