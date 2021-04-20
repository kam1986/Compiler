module internal DFA

#nowarn "25"
// just a junk function showing that we can make 'maybe equal' 
// let ( ?= ) a b = if a = b then Ok b else sprintf "%A is not equal %A" a b |> Error 
open Position
open Result
open Regex
open Iter
open Mapping
open TypeAcrobatics
open Token
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




let StateFinder regex =
    let mutable DTran = Map.empty
    let followpos =  Followpos regex Map.empty
    let language = GetLanguage regex
    let followposOf = fun item -> Map.find item followpos
    let correspondTo = fun item -> Map.find item (GetCorrespondence regex Map.empty)
    

    // use single linked list as two stacks.
    let mutable unmarked = [FirstPosOf regex]
    let mutable marked = []
    // the loop below are fine but not peak optimal in performance
    // should be changed to perform better
    while unmarked <> []  do // <> is equal to != in C#
        // the condition of the loop makes sure that this is always true.
        let (S :: unmarked') = unmarked
        unmarked <- unmarked' // mark first state
        for a in language do
            let U =
                Seq.fold 
                    (fun ps p -> ps + followposOf p) 
                    (set[]) 
                    (Set.intersect S (correspondTo a))
            // we do only add a new state to unmarked if it does not appear in both stacks
            if (not << Set.isEmpty) U then
                if not (Seq.contains U unmarked || Seq.contains U marked) then 
                    unmarked <- (U :: unmarked)
            
                DTran <- Map.add (S,a) U DTran
        if not <| Set.isEmpty S then

            marked <- List.distinct (S :: marked)
            
    
    // TODO: need to make the minimization algorithm of the DFA

    // we need language to know how large the transition table should be.
    language, List.rev marked, DTran


(*

    The code below, build a DFA transition table.
    an entry set to -1 simulate a illegal transition, at this point the DFA should stop
    and check for accepting, if not return an error, else return token.
    
    If a state in construction find that it has strictly more than 1 acceptance, it pick the one to return
    that has the earlist pattern in the list of patterns.

*)
    

let makeTable ((language : byte Set), (states : int Set list), transitions) =

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
                    // fix here
                    match Map.tryFind (state, letter) transitions with
                    | None -> ()
                    | Some destination -> 
                        let dest = Map.find destination states'
                        // find index of the transtion of the letter from the states offset
                        // size' * stateNumber finds the offset where the state begin, 
                        // and letter - min' is the offset from the starting position of the state
                        // where to be find the correct transition
                        // find the right state ofset to jump to and change the above entry to the state number
                        table.[index] <- byte <| dest 
            ) () states
        ) () language
    

    table, size', min'


// need the numbering of terminal to be in range -1 .. -n + 1 
let getAcceptancePrState (states : int Set seq) (acceptance : ('token * ('a -> 'b)) seq) =
    let states = Array.ofSeq states
    let acceptance = Array.ofSeq acceptance
    let acceptance' =
        Array.zip [|0 .. states.Length-1|] states                                                                                                               // number the states, assuming sorted list 
        |> Array.filter (fun (_, state) -> Set.exists (fun x -> x < 0) state)                                                                         // filter out all non acceptance state
        |> Array.map (fun (numb, state) -> numb, state |> Set.filter (fun x -> x < 0) |> Set.maxElement |> fun action -> acceptance.[-action-1])   // extract the acceptance action with hihgest precedence
        |> Map.ofArray                                                                                                                                          // return as a map
    

    // abusing the TryFind ability to give an option type back, hence automatical setting all none terminal states acceptance to None
    // and all terminal state to Some tranformation
    [| for entry in 0 .. states.Length-1 -> Map.tryFind entry acceptance' |]




// To minimize memory consumption, we use byte arrays since this give a minimal overbound on the table
// we still can define the same patterns encluding text encodings like utf7, utf8, utf16 and many more.
// it will encounter a slight hit in performance pr. letter since it in worst case can be 4 match rather than one
// in those cases, but this will be negated by the ability to make small jumps, rather than big once, making cache misses mutch less likely
// where cache missed offent are a bigger hit on performance.
let DfaMap (table : byte[]) size min' (accept : ('token * (string -> token)) option[])=
    
    fun (input : byte Iter) ->
        let mutable state = 0uy     // initial state
        let mutable index = 0       // not stickly needed, but should enforce no relocation on the register used for indexing.
        let mutable pos = Start
        let mutable noError = true
        let mutable token = None
        let mutable lexeme = ""
        let mutable msg = ""
        let mutable next = input
        while noError do
            match Next next with
            | Ok(c, next') ->
                index <- size * int state + int c - min'
                
                if index < size * (int state + 1) && index >= 0 && table.[index] <> ErrorTransitionMark then 
                    state <- table.[index]                  // transition to next state
                    lexeme <- lexeme + (string << char) c   // at letter to lexeme
                    next <- next'                           // updating position
                    match accept.[int state] with
                    | Some func -> 
                        token <- Some(func, lexeme, next)
                        pos <- next.GetPos
                    | _ -> ()
                else 
                    msg <- "Transition error"
                    noError <- false
            
            | Error msg' ->
                msg <- msg'
                noError <- false

        match token with
        // return token type as an integer
        // should check that the token type is correct
        | Some ((tokentype, func), l, next) -> 
            next.GetPos <- pos // rollback the position to last valid lexeme
            Ok(Token(tokentype, func l, GetPos next), next)
        | None -> Error <| msg
    |> Map
