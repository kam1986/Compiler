﻿module Iter

open Return
open Decoding
open Position

open System.IO

type 'I Iter =
    abstract member Next : Result<'I * 'I Iter, string>
    abstract member Prev : unit
    abstract member Show : unit
    abstract member IsEmpty : bool
    abstract member GetPos : Position byref

let Next (iter : 'I Iter) = iter.Next
let Prev (iter : 'I Iter) = iter.Prev
let Show (iter : 'I Iter) = iter.Show
let IsEmpty (iter: 'I Iter) = iter.IsEmpty
let GetPos (iter: 'I Iter) = iter.GetPos


[<Struct>]
type FromString =
    val private buf : byte list
    val mutable private pos : Position
    val mutable private spos : Position
    private new(xs, pos, spos) = {buf = xs; pos = pos; spos = spos}
    new(str : string) = { buf = GetBtyes str ; pos= Start ; spos= Start} 

    static member FromFile(path) =
        use file = System.IO.File.OpenText(path)
        FromString(file.ReadToEnd())

    interface byte Iter with
        member L.Next =
            match L.buf with
            | []      -> Error "End of Stream"
            | x :: xs when char x = '\n' ->
                Newline &L.pos
                Ok(x, FromString(xs, L.pos, L.spos) :> byte Iter)

            | x :: xs -> 
                Move &L.pos 1
                Ok(x, FromString(xs, L.pos, L.spos) :> byte Iter)
        
        member L.Prev = ()

        member L.Show = printfn "%A" L.buf

        member L.IsEmpty = List.isEmpty L.buf

        member L.GetPos = 
            let p = L.spos
            L.spos <- (Copy &L.pos) // will copy the position 
            match L.buf with
            | x :: _ when x = byte '\n'  -> Newline &L.spos
            | _                     -> Position.Next &L.spos
            p

[<Struct>]
type 'I SeqIter = 
    val private buf : 'I seq
    new(s) = { buf = s }

    interface 'I Iter with
        
        member I.Next =
            match Seq.tryHead I.buf with
            | None -> Error "End of Seq"
            | Some item -> Ok (item, SeqIter<'I>(Seq.tail I.buf) :> 'I Iter)

        member I.Prev = ()

        member I.Show = ()

        member I.IsEmpty = Seq.isEmpty I.buf

        
        member I.GetPos = Start


[<Struct>]
type 'I lst =
    val private buf : 'I list
        new(cs) = { buf = cs }
    interface 'I Iter with
        member L.Next =
            match L.buf with
            | []      -> Error "End of Stream"
            | x :: xs -> Ok(x, lst(xs) :> 'I Iter)
    
        member L.Prev = ()

        member L.Show = printfn "%A" L.buf

        member L.IsEmpty = List.isEmpty L.buf
    
        member I.GetPos = Start

/// buffer all content from a file to be iterated over by the lexer
/// not intented for big files.
[<Struct>]
type FromFile =
    val content : byte seq
    val line : int
    val offset : int
    val absolut : int
    private new(content, line, offset, absolut) = 
        {
            content = content
            line = line
            offset = offset
            absolut = absolut
        }

    new(path : string) = 
        {
            content = File.ReadAllBytes(path)
            line = 0
            offset = 0
            absolut = 0
        }

    interface byte Iter with
        member I.Next =
            match Seq.tryHead I.content with
            | None   -> Error "End of file"
            | Some b ->
                match char b with
                | '\n' ->
                    let s = FromFile(Seq.tail I.content, I.line + 1, 0, I.absolut + 1) :> byte Iter 
                    Ok(b, s)
            
                | '\t' -> 
                    let s = FromFile(Seq.tail I.content, I.line, I.offset+4, I.absolut + 1) :> byte Iter 
                    Ok(b, s)

                | _ -> 
                    let s = FromFile(Seq.tail I.content, I.line, I.offset+1, I.absolut + 1) :> byte Iter 
                    Ok(b, s)
        
        member I.Prev = ()

        member I.Show =
            I.content
            |> Seq.toArray
            |> System.Text.Encoding.Unicode.GetString
            |> printfn "%s"


        member I.IsEmpty = Seq.isEmpty I.content

        member I.GetPos = Start
