module Position


[<Struct>]
type Position = 
        val mutable Line : int 
        val mutable Offset : int 
        val mutable Indentation : int 
        val mutable Absolut : int
        internal new(line, offset, absolut) = {
            Line = line
            Offset = offset
            Indentation = offset >>> 2
            Absolut = absolut
            }

        

let Start = Position()

// for performance boost the code below needs to be handle internal instead

let Move (pos : Position byref) steps = 
        pos.Offset <- pos.Offset + steps 
        pos.Absolut <- pos.Absolut + steps
       
let Next (pos : Position byref) = Move &pos 1


let Newline (pos : byref<Position>) = 
    pos.Absolut <- pos.Absolut + 1
    pos.Line <- pos.Line + 1
    pos.Offset <- 0
    pos.Indentation <- 0
 
    
let Line (pos : Position byref) = pos.Line
let Offset (pos : Position byref) = pos.Offset
let Indentation (pos : Position byref) = pos.Indentation
let Absolut (pos : Position byref) = pos.Absolut
// make a copy and set indentation correctly
let Copy (pos: Position byref) = Position(Line &pos, Offset &pos, Absolut &pos)
