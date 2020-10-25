module Stack

open TypeAcrobatics


type Stack = 
    struct
        val mutable private stack : Arg[]
        val mutable private count : int
        new(_) = 
            { 
                stack = [|null|] 
                count = 0
            }
        
        member s.Push item =
            if s.count < s.stack.Length then
                s.stack.[s.count] <- (item :> obj)
            else
                let sz = s.stack.Length * 2
                let stack = [|for _ in 1 .. sz -> null|]
                for i in 0 .. s.stack.Length - 1 do
                    stack.[i] <- s.stack.[i]
                s.stack <- stack

        
    end