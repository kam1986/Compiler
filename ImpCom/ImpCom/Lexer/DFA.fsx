module DFA

// just a junk function showing that we can make 'maybe equal' 
// let ( ?= ) a b = if a = b then Ok b else sprintf "%A is not equal %A" a b |> Error 

open Regex


(* 
    For now we use Maximum value of a byte as error mark.
    This is do to the ASCII encoding uses only the 0 - 127 interval
    And UFT8 can not have a full set byte in a sequence

    i.e.
    ASCII byte 0xxxxxxx.
    UTF8 byte sequence have following format where x is either set ot not
        single byte : 0xxxxxxx
        double byte : 110xxxxx 10xxxxxx
        triple byte : 1110xxxx 10xxxxxx 10xxxxxx
        fuour  byte : 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx

    This might be change since there is a lot of other bits invalid byte to be used as mark 
*)

let private ErrorTransitionMark = 255uy

let rec GetLanguage regex =
    match regex with
    | regex.Atom (a, _) -> set[a]
    | regex.Cat (regex1, regex2, _, _, _) | regex.Or (regex1, regex2, _, _, _) ->
        GetLanguage regex1 + GetLanguage regex2
    | regex.Star(regex, _, _) -> GetLanguage regex
    | _ -> set[]

let rec GetCorrespondence regex correspond =
    match regex with
    | regex.Atom (a, i) ->
        match Map.tryFind a correspond with
        | None -> Map.add a (set[i]) correspond
        | Some c -> Map.add a (set[i] + c) correspond
    
    | regex.Cat (regex1, regex2, _, _, _) | regex.Or (regex1, regex2, _, _, _) ->
        GetCorrespondence regex1 correspond
        |> GetCorrespondence regex2

    | Star (regex, _, _) ->
        GetCorrespondence regex correspond

    | _ -> correspond


// OBS the code below are hurtfully inefficient 

let mp = Map.empty |> Map.add 1 7


let GetAcceptanceStates states =
    List.zip states [ 0 .. states.Length - 1 ] // add state number to each state
    |> List.filter (fun (state, _) -> Set.exists (fun x -> x < 0) state)        // find all with acceptance (i < 0)
    |> List.map (fun (state, numb) -> Set.filter (fun x -> x < 0) state, numb)  // filter none terminals from
    |> List.map (fun (state, numb) -> numb, Set.maxElement state)               // find highest predences of acceptance.

let StateFinder regex =
    let mutable DTran = Map.empty
    let followpos =  Followpos regex Map.empty
    let language = GetLanguage regex
    let followposOf = fun item -> Map.find item followpos
    let correspondTo = fun item -> Map.find item (GetCorrespondence regex Map.empty)
    

    // use single linked list as two stacks.
    let mutable unmarked = set[FirstPosOf regex]
    let mutable marked = set[]
    // the loop below are fine but not peak optimal in performance
    // should be changed to perform better
    while not <| Seq.isEmpty unmarked  do // <> is equal to != in C#
        // the condition of the loop makes sure that this is always true.
        let (S :: unmarked') = List.ofSeq unmarked
        printfn "S is; %A" S
        unmarked <- set unmarked' // mark first state
        for a in language do
            let U = 
                Seq.fold 
                    (fun ps p -> ps + followposOf p) 
                    (set[]) 
                    (Set.intersect S (correspondTo a))
            // we do only add a new state to unmarked if it does not appear in both stacks
            if not (Seq.contains U unmarked || Seq.contains U marked)
            then 
                unmarked <- Set.add U unmarked
            DTran <- Map.add (S,a) U DTran
        marked <- Set.add S marked
            
    
    // TODO: need to make the minimization algorithm of the DFA

    
    // we need language to know how large the transition table should be.
    language, marked, DTran


(*

    The code below, build a DFA transition table.
    an entry set to -1 simulate a illegal transition, at this point the DFA should stop
    and check for accepting, if not return an error, else return token.
    
    If a state in construction find that it has strictly more than 1 acceptance, it pick the one to return
    that has the earlist pattern in the list of patterns.

*)
    

let makeTable (language : byte Set) (states : int Set Set) (transitions: Map<(int Set * byte), int Set>) =
    // label states with numbers from 0 to n-1, where n is the number of states
    let states' = Map.ofSeq <| Seq.zip states [ 0 .. Seq.length states - 1 ]

    // find the minimum and maximum offset of the language
    let min', max' = int <| Set.minElement language, int <| Set.maxElement language
    // finde the distance between those two.
    let size' = max' - min' + 1

    // transition table starting by assuming all transition leads to error
    let table = [| for _ in 1 .. size' * Seq.length states -> ErrorTransitionMark |]
    // inserting legal transitions
    // using Seq. to enable change og collection in the input
    Seq.fold ( 
        fun _ letter -> 
            Seq.fold (
                fun _ state -> 
                    let src = Map.find state states'
                    let index = size' * src + (int letter) - min' 
                    let dest = Map.find (state, letter) transitions |> fun destination -> Map.find destination states'
                    // find index of the transtion of the letter from the states offset
                    // size' * stateNumber finds the offset where the state begin, 
                    // and letter - min' is the offset from the starting position of the state
                    // where to be find the correct transition
                    // find the right state ofset to jump to and change the above entry to the state number
                    table.[index] <- byte <| dest 
            ) () states
        ) () language

    

    table, size'

    