module Position


[<Struct>]
type Position = 
    { 
        mutable Line : int 
        mutable Offset : int 
        mutable Indentation : int 
        mutable Absolut : int
    }

let Start = 
    { 
        Line = 0
        Offset = 0
        Indentation = 0
        Absolut = 0
    }

// for performance boost the code below needs to be handle internal instead

let Move pos steps = 
    let offset = pos.Offset + steps
    { pos with 
        Offset = offset 
        Absolut = pos.Absolut + steps
        Indentation = offset / 4 // set indentation to enhance performance on indentation grammar
    }

let Next pos = Move pos 1

let Newline (pos : byref<Position>) = 
    pos.Absolut <- pos.Absolut + 1
    pos.Line <- pos.Line + 1
    pos.Offset <- 0
    pos.Indentation <- 0
    pos
    

let Line (pos : Position) = pos.Line
let Offset (pos : Position) = pos.Offset
let Indentation (pos : Position) = pos.Indentation
let Absolut (pos : Position) = pos.Absolut
