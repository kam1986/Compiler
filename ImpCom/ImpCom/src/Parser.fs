module Parser

open Position
open Token
open Productions
open TypeAcrobatics
open NFA
#nowarn "25"
(*      

    First thoughs:

    The parser should take a list of rules, where a rule is a a list of patterns of lists of tokens/rules 
    We label each token type by a byte(or larger), then we label each rule by a number
    We then use this numbers to make a NFA over the rules, which we then convert to a DFA
    Followed by each rule should be a transformation function to apply to the rules return type of the form
    
        fun args : Args[] -> f (Take args.[i] ... Take args.[j])
        or (func,(i, ... ,j)) ? maybe add 'valueOf i' and 'PosOf j' functions to add readability to the parser

    This is needed because we can't just pass an arbitrary tuple with no prior information. (The array will most likely be optimized away anyway)

    After we got the normal parser to work we implement indentation sensitive parsing into it.

*)



type Action =
    | Shift     of int
    | Reduce    of int
    | Goto      of int
    | Error     
    | Accept




let text input =
    match input with 
    | Shift n -> sprintf "s%d" n
    | Reduce n -> sprintf "r%d" n
    | Accept -> " a"
    | Error -> "  "
    | Goto n -> sprintf "g%d" n

let print row size (table : _[]) =

    for i in 0 .. row .. size-1 do
        let i' = i/row

        printfn "%d %s" (i/row) (Array.reduce (fun str s -> str + " " + s)  <| Array.map text table.[i .. row+i - 1])




let GetLanguage (Productions productions) =
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
   




let rec findaction state actions =
    match actions with
    | [] -> failwith "actions missing"
    | (s, p, _, _) :: _ when s = state -> p
    | _ :: actions -> findaction state actions



let makeTable follow actions language trans dfa =
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

        match Set.contains 1 state with
        | true -> 
            table.[size*src] <- Accept
        | _ ->
            for symbol in language do
                // check if ther is a transition from the current state on the current symbol
                let accept = Set.intersect state acceptingstates
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
                            let p = (accept - (set[1])).MinimumElement
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
  


let ItemsToPop (Productions productions) actions =
    List.map (fun (Production(_, (rules : (Symbol<int,int> list * (Token<_>[] -> token))list))) -> 
        List.map (fun (rule, _) -> List.length rule) rules) productions
    |> fun lst -> List.foldBack (fun lst l -> lst @ l) lst []
    |> fun items -> List.zip items actions
    |> List.toArray

let CorrectReduce (table: _ []) (actions: _ list) =
    for i in 0 .. table.Length - 1 do
        for (rule, nfastate) in actions do
            match table.[i] with
            | Reduce n when n = nfastate -> table.[i] <- Reduce rule
            | _ -> ()
            
 


[<Struct>]
type SLR =
    val table : Action[]
    val size : int
    val symbols : int
    val actions : (int * int * (Token<int>[] -> token))[]
    new(productions) = 
        let lang = GetLanguage productions
        let size = lang.Count
        let symbols = lang |> Set.filter (fun t -> match t with NonTerminal _ -> false | _ -> true) |> Set.count
        let (NFA n as nfa), actions = MakeNFA productions
        let trans, dfa = MakeDFA nfa lang
        let flw = Follow productions
     
        // make an array of production number and action function
        let actions' = 
            List.map (fun (_, _, p, a) -> (p, a)) actions 
            |> ItemsToPop productions 
            |> Array.map (fun (pops,(production, action)) -> (pops, production, action))
        let table = makeTable flw actions lang trans dfa

        print (table.Length/dfa.Length) table.Length table
        printfn "actions %A" actions'
        { table = table ; size = size ; symbols = symbols ; actions = actions'}
    
    member private P.Action state input = P.table.[state * P.size + input.GetHashCode() + 1]
    member private P.Goto state input =  P.table.[state * P.size + P.symbols + input.GetHashCode()+1]


    /// get parse table
    member P.Table = P.table
        
         

    /// Get or Set specific entry in the parse table.
    /// This enable to help costumize the parser explicitly, but still give the value of generation the
    /// basic parse table.
    member P.Item
        with get(state, offset) = P.table.[state * P.size + offset]
        and set(state, offset) value = P.table.[state*offset] <- value

    member P.Parse input =
        let input = 
            Seq.map (fun (Token(tp, data, pos)) -> Token(tp.GetHashCode(), data, pos)) input
            |> fun s -> Seq.append s (seq { Token(-1, Arg null, Position.Start) })

        let mutable stack = []
        let mutable states = [0]
        let mutable current = input
        let mutable input = Seq.head current
        let mutable NoError = true
        let mutable err = ""
        while states <> [] && NoError do
            
            match P.Action states.Head (TypeOf input) with
            | Shift n ->
                printfn "Shift %d" n
                // this should be made alot more effecient by clever indexing of array based stack
                stack <- input :: stack
                states <- n :: states
                current <- Seq.tail current
                input <- Seq.head current

            | Reduce p ->
                printfn "Reduce %d" p
                // need to fix production
                let pops, production, action = P.actions.[p]
                let args = List.take pops stack |> List.rev |> List.toArray
                let value = action args
                
                stack <- Token(production, value, PosOf input) :: List.skip pops stack
                states <- List.skip pops states
                
                match P.Goto states.Head production with
                | Goto g ->
                    printfn "goto %d" g
                    states <- g :: states

                   
                | _ -> // should never be matched
                    printfn "states %A" states
                    NoError <- false
                    err <- "Parser Error at position " + string (PosOf input |> Line, PosOf input |> Offset)
                   

            | Accept ->
                printfn "accept"
                states <- []

            | _ -> 
                err <- "Parser Error at position " + string (PosOf input |> Line, PosOf input |> Offset) 
                printfn "states %A" states
                NoError <- false
            printfn ""

        match NoError with
        | false -> 
            printfn "%s" err
            printfn "current %A" current
            printfn "states %A" states
            exit -1
        | _ -> 
            List.head stack 
            |> ValueOf 
            
    
     
    static member Create (p : Productions<int,'T,'N>) =
        p
        |> ToCommonRepresentation
        |> addEndOfParse
        |> SLR



[<Struct>]
type LR1 = 
    val table : Action[]
    val size : int
    val symbols : int
    val actions : (int * int * (Token<int>[] -> token))[]

// fix me
let first nfa B =
    Map.toSeq nfa
    |> Seq.filter (fun ((i, _), _) -> B = i)
    |> Seq.map (fun ((_,l),_) -> l)
    |> Set.ofSeq

let Closure first language nfa I = 
    let mutable I' = I
    let mutable cond = true
    while cond do
        let old = I'
        for (i, a) in I do
            for l in language do
                match Map.tryFind (i, l) nfa with
                | None -> ()
                | Some B ->
                    for b in first nfa B do // need fixing
                        I' <- Set.add (B, b) I'
        cond <- I' <> old
    I'
    

let Goto nfa I X =
    let mutable J = Set.empty
    // instead of i represent the dot in the production 
    for (i, a) in I do
        // find all production of the from A -> a*Xb
        match Map.tryFind (i,X) nfa with
        | None -> ()
        | Some i' ->
            J <- Set.add (i',a) J
    Closure nfa J

let MakeLR1Table first nfa =