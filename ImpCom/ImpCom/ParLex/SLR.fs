
module SLR

open Position
open Token
open Productions
open TypeAcrobatics
open NFA
#nowarn "25"

type Action =
    | Shift     of int
    | Reduce    of int
    | Goto      of int
    | Error     
    | Accept




let internal text input =
    match input with 
    | Shift n -> sprintf "s%d" n
    | Reduce n -> sprintf "r%d" n
    | Accept -> " a"
    | Error -> "  "
    | Goto n -> sprintf "g%d" n

let internal print row size (table : _[]) =

    for i in 0 .. row .. size-1 do
        let i' = i/row

        printfn "%d %s" (i/row) (Array.reduce (fun str s -> str + " " + s)  <| Array.map text table.[i .. row+i - 1])




let internal GetLanguage (Productions productions) =
    let rec gl language productions =
        match productions with
        | [] -> language
        | Production(N, ra) :: productions ->
            let rules, _ = List.unzip ra
            let lang = 
                List.fold (fun lang rule -> 
                    List.fold (fun lang symbol ->
                        Set.add symbol lang
                    ) lang rule
                ) language rules
            gl (Set.add (NonTerminal N) lang) productions

    gl Set.empty productions
   


let rec internal findaction state actions =
    match actions with
    | [] -> failwith "actions missing"
    | (s, p, _, _) :: _ when s = state -> p
    | _ :: actions -> findaction state actions



let internal makeTable follow actions language trans dfa =
    // finding all terminals
    let terminals = 
        Set.filter (fun symbol -> match symbol with Terminal _ -> true | _ -> false) language
        |> Set.map (fun (Terminal c) -> c)

    let min', max' = (Set.minElement terminals).GetHashCode(), (Set.maxElement terminals).GetHashCode()
    let symbols = max' + 2 // include both end to the interval by adding one
    let size = language.Count// removing nonterminal from addendofparse
    let numberofstates = List.length dfa// removing the added state for acceptance
    // assume error at first
    let table = [| for _ in 1 .. size * numberofstates -> Error |]

    let states = 
        dfa
        |> List.fold (fun (map, state) dstate -> 
            Map.add dstate state map, state+1
            ) (Map.empty, 0)
        |> fst

    // making accepting state set
    let acceptingstates = 
        List.map (fun (x, _, _, _) -> x) actions 
        |> Set.ofList
    for state in dfa do
        let src = Map.find state states

        if Set.contains 1 state then
            table.[size*src] <- Accept
        
        for symbol in language do
            // check if ther is a transition from the current state on the current symbol
            let accept = Set.intersect state acceptingstates - (set[1])
            let offset = size * src
            match Map.tryFind (state, symbol) trans with
            | None ->
                    
                match symbol with
                | NonTerminal N when N <> -1 -> 
                    let flw = 
                        Map.find N follow 
                        |> Set.map (fun (Terminal c) -> c)

                    match accept.Count with
                    | 0 ->
                        ()
              
                    | 1 -> 
                        let p = accept.MinimumElement

                        let p' = findaction p actions
                        for c in flw do
                            table.[offset + c + 1] <- Reduce p'
                        
                    | _ ->
                        let p = accept.MinimumElement
                        let p' = findaction p actions
                        for c in flw do
                            table.[offset + c + 1] <- Reduce p'
                    
                | _ -> ()

            | Some s ->
                let dst = Map.find s states            
                match symbol with
                | Terminal c -> 
                    let offset = size * src + 1 + c // shifting the table entries by one to the right, this allow the terminal to be represented by - 1 
                    table.[offset] <- Shift dst
                        
                | NonTerminal N ->
                    // getting acceptance nfa states of the dfa state
                    let flw = 
                        Map.find N follow 
                        |> Set.map (fun (Terminal c) -> c)

                    match accept.Count with
                    | 0 -> 
                        let offset = offset + symbols + N
                        if table.[offset] <> Accept then
                            table.[offset] <- Goto dst
                            
                    | 1 -> 
                        let p = accept.MinimumElement
                        let p' = findaction p actions
                        table.[offset + symbols + N] <- Goto dst
                        for c in flw do
                            table.[offset + c + 1] <- Reduce p'
                        
                    | _ ->
                        let p = (accept - (set[1])).MinimumElement
                        let p' = findaction p actions
                            
                        table.[offset + symbols + N] <- Goto dst
                        for c in flw do
                            table.[offset + c + 1] <- Reduce p'
                   

    // TODO use 'trans' to implement the shift, reduce, accept into the table                 
    table
  


let internal ItemsToPop (Productions productions) actions =
    List.map (fun (Production(_, (rules : (Symbol<int,int> list * (Token<_>[] -> token))list))) -> 
        List.map (fun (rule, _) -> List.length rule) rules) productions
    |> fun lst -> List.foldBack (fun lst l -> lst @ l) lst []
    |> fun items -> List.zip items actions
    |> List.toArray

let internal CorrectReduce (table: _ []) (actions: _ list) =
    for i in 0 .. table.Length - 1 do
        for (rule, nfastate) in actions do
            match table.[i] with
            | Reduce n when n = nfastate -> table.[i] <- Reduce rule
            | _ -> ()
            
 
let internal SLR productions =
    let productions =
        productions
        |> ToCommonRepresentation
        |> addEndOfParse

    let lang = GetLanguage productions
    let size = lang.Count
    let symbols = lang |> Set.filter (fun t -> match t with NonTerminal _ -> false | _ -> true) |> Set.count
    let mutable (NFA n as nfa), actions = MakeNFA productions
    let trans, dfa = MakeDFA nfa lang
    let flw = Follow productions
    // make an array of production number and action function
    let actions' = 
        List.map (fun (_, _, p, a) -> (p, a)) actions 
        |> ItemsToPop productions 
        |> Array.map (fun (pops,(production, action)) -> (pops, production, action))
    let mutable table = makeTable flw actions lang trans dfa
        
    (table, size, symbols, actions')
    
  
