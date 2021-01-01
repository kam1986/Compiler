module internal LR1

open Productions
open NFA

let IsTerminal =
    function
    | Terminal _ -> true
    | _ -> false


let First nullable first nfa b (lookahead : _ Set) =
   match b with
   | Terminal a -> set[a]
   | NonTerminal N ->
        let fst = Map.find N first
        if Map.find N nullable then
           lookahead + fst
        else
           fst


    
(*
    new closure should be
        let mutable I' = I
        foreach (state, lookahesd) in I do
            let cls = epsilonclosure(state) // only distinction between the old and new closure is the applied lookahead
            foreach i in cls do
                foreach l in first(i, lookahead) do // where first do one goto 
                    add (i, l) to I' 
*)