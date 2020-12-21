module Iter

open Return
open Decoding
open Position

open System.IO

type 'I Iter =
    abstract member Next : Result<'I * 'I Iter, string>
    abstract member Prev : unit
    abstract member Show : unit
    abstract member IsEmpty : bool
    abstract member GetPos : Position

let Next (iter : 'I Iter) = iter.Next
let Prev (iter : 'I Iter) = iter.Prev
let Show (iter : 'I Iter) = iter.Show
let IsEmpty (iter: 'I Iter) = iter.IsEmpty
let GetPos (iter: 'I Iter) = iter.GetPos


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
            | []      -> Error "End of Stream"
            | x :: xs -> Ok(x, List(xs) :> byte Iter)
        
        member L.Prev = ()

        member L.Show = printfn "%A" L.buf

        member L.IsEmpty = List.isEmpty L.buf

        member I.GetPos = Start

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

        member I.GetPos = 
            {
                Absolut = I.absolut
                Line = I.line
                Offset = I.offset
                Indentation = I.line / 4 // needed for identation based parsing (yet to be implementet)
            }

