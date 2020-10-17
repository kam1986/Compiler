module Regex

(*

    This is an none pure ASCII lexer the transformation from char to byte is strictly to make sure that 
    we minimize memory compsumption.

    [â ... ] will make a complement of the interval given by ... with respect to the ASCII character set where
    [^ ... ] will make a complement of the interval given by ... of the full span of byte.
    ??


    OBS: it is possible to make a lexer from it that takes pattern of other encoding.
    but for the moment it is to be hand made, by encoding all possible patterns of UTF-8 or so as regex.


    TODO: Interval and complements
*)

open Result


// 
let private ASCII    = set[0uy .. 127uy]
let private All = set[0uy .. 255uy]
type regex =
    | Epsilon
    | Terminal of int
    | Atom of byte * int
    | Cat of regex * regex * int Set * int Set * bool
    | Or of regex * regex * int Set * int Set * bool 
    | Star of regex * int Set * int Set

// Construction functions

let IsNullAble reg =
    match reg with
    | Epsilon | Star _ -> true
    | Atom _ | Terminal _ -> false
    | Cat (_, _, _, _, n) | Or (_, _, _, _, n) -> n


let FirstPosOf reg =
    match reg with
    | Epsilon -> set[]
    | Atom (_, i) | Terminal i -> set[i]
    | Cat (_, _, fp, _, _) | Or (_, _, fp, _, _) | Star (_, fp, _) -> fp


let LastPosOf reg =
    match reg with
    | Epsilon -> set[]
    | Atom (_, i) | Terminal i -> set[i]
    | Cat (_, _, _, lp, _) | Or (_ , _, _, lp, _) | Star (_, _, lp) -> lp

let rec GetTerminals regex =
    match regex with
    | regex.Terminal i -> Set[i]
    | regex.Atom _ |Epsilon -> set[]
    | regex.Star(regex, _, _) -> GetTerminals regex
    | regex.Or(regex1, regex2, _ ,_, _) | regex.Cat(regex1, regex2, _ ,_, _) ->
        GetTerminals regex1 + GetTerminals regex2


let rec Followpos regex fp =
    match regex with
    | Cat(c1, c2, _, _, _) -> 
        Followpos c1 fp // compute followpos for the left side subtree
        |> Followpos c2 // compute followpos for the right side subtree
        |> Set.foldBack ( // compute followpos for this node
            fun i fp ->
                match Map.tryFind i fp with
                | None -> Map.add i (FirstPosOf c2) fp
                | Some f -> Map.add i (f + FirstPosOf c2) fp
        ) (LastPosOf c1)

    | Star(n, f, l) ->
        Followpos n fp // compute followpos for the subtree
        |> Set.foldBack ( // compute followpos for this node
            fun i fp ->
                match Map.tryFind i fp with
                | None -> 
                   Map.add i f fp
                | Some f' -> Map.add i (f' + f) fp
        ) l
    | Or(c1, c2, _, _, _) ->
        Followpos c1 fp // compute followpos for the left side subtree
        |> Followpos c2 // compute followpos for the right side subtree

    | _ -> fp // leaf cases 





// easy wrappers to hide the ugly stuff
let Cat reg1 reg2 =
    let fp = 
        if reg1 |> IsNullAble 
        then FirstPosOf reg1 + FirstPosOf reg2
        else FirstPosOf reg1
    
    let lp =
        if reg2 |> IsNullAble 
        then LastPosOf reg1 + LastPosOf reg2
        else LastPosOf reg2

    let n = IsNullAble reg1 && IsNullAble reg2

    Cat(reg1, reg2, fp, lp, n)

let Or reg1 reg2 =
    let fp = FirstPosOf reg1 + FirstPosOf reg2
    let lp = LastPosOf reg1 + LastPosOf reg2
    let n = IsNullAble reg1 || IsNullAble reg2

    Or(reg1, reg2, fp, lp, n)



let Star reg = Star(reg, FirstPosOf reg, LastPosOf reg) 

// do not need to rename the atoms ID
let Plus reg = Cat reg <| Star reg





(* ---------------------- Regex Lexer ---------------------------- 

    Two face parsing of regex 1. tokizing 2. construction
    This is done to make it possible to construct a regex parser as a 
    function represenation of that regex
    a DFA representation of that regex
    or NFA representation of that regex

*)
open Mapping
open Iter

// minimal token type
type RegexToken =
    | Atom of byte
    | Epsilon
    | OR 
    | CAT
    | STAR
    | PLUS

let IsA col a = Seq.contains a col
let IsNotA col a = IsA col a |> not

let keyword = 
    set['\\'; '|'; '*'; '+'; '-'; '['; ']'; '('; ')'; '^'; 'â']
    |> Set.map byte

let RegexError msg =
    sprintf "Regex Error:\n\t%s" msg
    |> Failure

// Lexing after a legal atom
let Atom =
    fun input ->
        match Next input with
        | Failure msg -> Failure msg
        | Success (b, iter) when b |> IsNotA keyword  -> 
            Success(RegexToken.Atom b, iter)
        | Success (b, iter) ->
            Prev iter
            sprintf "Expected an atom but got %c" <| char b
            |> RegexError
    |> Map
    |> (>>) (fun reg -> [reg])


// Lexing after some predicate function to be true
let inline Expect pred  =
    fun input ->
        match Next input with
        | Failure msg -> Failure msg
        | Success (b, iter) when pred b -> 
            Success(b, iter)
        | Success (_, iter) ->
            Prev iter
            sprintf "Error in parsing " 
            |> RegexError
    |> Map


// TODO need to add more general escape keys
// Lexing for escape atoms
let Escape =
    let pattern =
        Expect (fun b -> b = byte '\\') <&> Expect (fun _ -> true)
        |>  (>>) (fun (_, b) -> b)
    fun input ->
        match Run pattern input with
        | Failure msg -> Failure msg
        | Success (116uy, iter) ->
            Success(RegexToken.Atom 9uy, iter)
        | Success (110uy, iter) ->
            Success(RegexToken.Atom 10uy, iter)
        | Success (114uy, iter) ->
            Success(RegexToken.Atom 13uy, iter)
        | Success (b, iter) ->
            Success(RegexToken.Atom b, iter)
    |> Map
    |> (>>) (fun reg -> [reg])


let Interval =
    let rec inner input =
        ((Atom <|> Escape) <&> Expect (fun b -> b = byte '-') <&> (Atom <|> Escape)) 
        |> (>>) (fun (([RegexToken.Atom a], _), [RegexToken.Atom b]) -> 
                if a < b 
                then set[a..b]
                else set[b..a]                
            )
        |> fun pattern -> pattern <|> ((fun [Atom a] -> set[a]) >> Atom)
        |> fun pattern -> (fun (s1, s2) -> s1 + s2) >> (pattern <&> (Map inner)) <|> pattern // elemination doublicates
        |> fun pattern -> Run pattern input
    
    let oring lst =
        match lst with
        | [] -> []
        | [x] -> [x]
        | x :: xs -> List.fold (fun acc a -> OR :: a :: acc) [x] xs

    Expect (fun b -> b = byte '[') <&> Map inner <&> Expect (fun b -> b = byte ']')
    |> (>>) (fun ((_,regex),_) -> 
        regex
        |> Seq.map (fun a -> RegexToken.Atom a)
        |> Seq.toList
        |> oring
    )


let Complement =
    let rec inner input =
        ((Atom <|> Escape) <&> Expect (fun b -> b = byte '-') <&> (Atom <|> Escape)) 
        |> (>>) (fun (([RegexToken.Atom a], _), [RegexToken.Atom b]) -> 
                if a < b 
                then set[a..b]
                else set[b..a]                
            )
        |> fun pattern -> pattern <|> ((fun [Atom a] -> set[a]) >> Atom)
        |> fun pattern -> (fun (s1, s2) -> s1 + s2) >> (pattern <&> (Map inner)) <|> pattern // elemination doublicates
        |> fun pattern -> Run pattern input
    
    let oring lst =
        match lst with
        | [] -> []
        | [x] -> [x]
        | x :: xs -> List.fold (fun acc a -> OR :: a :: acc) [x] xs

    Expect (fun b -> b = byte '[') <&> Expect (fun b -> b = byte '^') <&> Map inner <&> Expect (fun b -> b = byte ']')
    |> (>>) (fun ((_,regex),_) -> 
        All - regex // taking the compliement
        |> Seq.map (fun a -> RegexToken.Atom a)
        |> Seq.toList
        |> oring
    )


let ASCIIComplement =
    let rec inner input =
        ((Atom <|> Escape) <&> Expect (fun b -> b = byte '-') <&> (Atom <|> Escape)) 
        |> (>>) (fun (([RegexToken.Atom a], _), [RegexToken.Atom b]) -> 
                if a < b 
                then set[a..b]
                else set[b..a]                
            )
        |> fun pattern -> pattern <|> ((fun [Atom a] -> set[a]) >> Atom)
        |> fun pattern -> (fun (s1, s2) -> s1 + s2) >> (pattern <&> (Map inner)) <|> pattern // elemination doublicates
        |> fun pattern -> Run pattern input
    
    let oring lst =
        match lst with
        | [] -> []
        | [x] -> [x]
        | x :: xs -> List.fold (fun acc a -> OR :: a :: acc) [x] xs

    Expect (fun b -> b = byte '[') <&> Expect (fun b -> b = byte 'â') <&> Map inner <&> Expect (fun b -> b = byte ']')
    |> (>>) (fun ((_,regex),_) -> 
        ASCII - regex // taking the compliement of in respect to the ascii character set
        |> Seq.map (fun a -> RegexToken.Atom a)
        |> Seq.toList
        |> oring
    )


// Parsin paranteses
let Paranteses pattern =
    Expect (fun b -> b = byte '(') <&> pattern <&> Expect (fun b -> b = byte ')')
    |> (>>) (fun ((_, regex), _) -> regex)

// Parsing or regex over some pattern and prefixing the or for simpler parsing to syntaks tree
let orr pattern =
    let rec o input =
        (fun ((reg1, _), reg2) -> OR :: reg1 @ reg2) >> (pattern <&> Expect (fun b -> b = byte '|') <&> Map o)
        |> fun pattern' -> pattern' <|> pattern
        |> fun pattern' -> Run pattern' input
    Map o

let cat pattern =
    let rec c input =
        (fun (reg1, reg2) -> CAT :: reg1 @ reg2) >> (pattern <&> Map c)
           |> fun pattern' -> pattern' <|> pattern
           |> fun pattern' -> Run pattern' input
    Map c


// Parsing star regex over some pattern and prefixing the or for simpler parsing to syntaks tree
let star pattern =
    pattern <&> Expect (fun b -> b = byte '*')
    |> (>>) (fun (reg, _) -> STAR :: reg)


// Parsing plus regex over some pattern and prefixing the or for simpler parsing to syntaks tree
let plus pattern =
    pattern <&> Expect (fun b -> b = byte '+')
    |> (>>) (fun (reg, _) -> PLUS :: reg)

let Primitives pattern = Paranteses pattern <|> Interval <|> Complement <|> ASCIIComplement <|> Escape <|> Atom

let starPlusPrim pattern =
    star (Primitives pattern) <|> plus (Primitives pattern) <|> Primitives pattern

let Tokenizer = 
    let rec regex input =
        // The star/plus has higher precedence than orr and orr has higher precendence than cat hence the order.
        cat (orr (starPlusPrim (Map regex)))
        |> fun pattern -> Run pattern input
    Map regex


// --------------------------------------- Regex Parser --------------------------------- //

let Parser =
    // internal mutability is okay
    let mutable count = 0
    let atom = 
        fun input ->
            match Next input with
            | Success (Atom a, iter) ->
                let a' = regex.Atom(a, count)
                count <- count + 1
                Success(a', iter)
            | _ ->
                Failure "Parser Error: not an atom"
        |> Map

    let star pattern =
        Expect (fun token -> token = STAR) <&> pattern
        |> (>>) (fun (_, regex) -> Star regex)
        

    let plus pattern =
        Expect (fun token -> token = PLUS) <&> pattern
        |> (>>) (fun (_, regex) -> Plus regex)
        

    let Cat pattern =
        Expect (fun token -> token = CAT) <&> pattern <&> pattern
        |> (>>) (fun ((_, regex1), regex2) -> Cat regex1 regex2)
        

    let Orr pattern =
        Expect (fun token -> token = OR) <&> pattern <&> pattern
        |> (>>) (fun ((_, regex1), regex2) -> Or regex1 regex2)
       
        

    let Regex =
        let rec regex input =
            Orr (Map regex) <|> Cat (Map regex) <|> star (Map regex) <|> plus (Map regex) <|> atom
            |> fun pattern -> Run pattern input
        Map regex
    fun input ->
        match Run Regex input with
        | Success (regex, iter) -> Success((regex, count), iter)
        | Failure msg -> Failure msg
    |> Map

    
