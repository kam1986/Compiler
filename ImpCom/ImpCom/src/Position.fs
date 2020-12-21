module Position


[<Struct>]
type Position = 
    { 
        Line : int 
        Offset : int 
        Indentation : int 
        Absolut : int
    }

let Start = 
    { 
        Line = 0
        Offset = 0
        Indentation = 0
        Absolut = 0
    }

let Move pos steps = 
    let offset = pos.Offset + steps
    { pos with 
        Offset = offset 
        Absolut = pos.Absolut + steps
        Indentation = offset / 4 // set indentation to enhance performance on indentation grammar
    }

let Next pos = Move pos 1

let Newline pos = 
    { 
        Absolut = pos.Absolut + 1
        Line = pos.Line + 1
        Offset = 0
        Indentation = 0
    }

let Line (pos : Position) = pos.Line
let Offset (pos : Position) = pos.Offset
let Indentation (pos : Position) = pos.Indentation
let Absolut (pos : Position) = pos.Absolut