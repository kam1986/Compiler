module testing

open Parser
open Productions
open NFA
open TypeAcrobatics
    
let table = 
    [|  
    //     $         a        b        c        T       R
        Reduce 3; Shift 3; Shift 4; Reduce 3; Goto 1; Goto 2    // 0 - 6
        Accept  ; Error  ; Error  ; Error   ; Error ; Error     // 1 - 12
        Reduce 1; Error  ; Error  ; Reduce 1; Error ; Error     // 2 - 18
        Reduce 3; Shift 3; Shift 4; Reduce 3; Goto 5; Goto 2    // 3 - 24
        Reduce 3; Error  ; Shift 4; Reduce 3; Error ; Goto 6    // 4 - 30
        Error   ; Error  ; Error  ; Shift 7 ; Error ; Error     // 5 - 36
        Reduce 4; Error  ; Error  ; Reduce 4; Error ; Error     // 6 - 42
        Reduce 2; Error  ; Error  ; Reduce 2; Error ; Error     // 7 - 48
    |]
    
let r p =
    match p with
    | 0 
    | 1 
    | 2 -> 0
    | 3
    | 4 -> 1
    | _ -> failwith "something went wrong"
    
let mutable input = [A;A;B;B;B;C;C] |> List.map (fun f -> f.GetHashCode()) |> fun lst -> lst @ [-1]
let mutable stack = [0]
     
    
let reset() =
    input <- [A;A;B;B;B;C;C] |> List.map (fun f -> f.GetHashCode()) |> fun lst -> lst @ [-1]
    stack <- [0]
    
let Action (table : _[]) size state input = table.[state * size + input.GetHashCode() + 1]
let GoTo (table : _[]) size symbols state input = table.[state * size + symbols + input.GetHashCode()]
    
    
let pop p  =
    match p with
    | 0 -> 0
    | 1 -> 2
    | 2 -> 6
    | 3 -> 0
    | 4 -> 4
    | _ -> failwith "something went wrong"
    
// example of parsing algorithm
let s1() =
    match Action table 6 stack.Head input.Head with
    | Shift n ->
        printfn "shift %d" n
        stack <- input.Head :: stack
        stack <- n :: stack
        input <- input.Tail
        printfn "input: %A\nstack: %A" input stack
    
    | Reduce p ->
        printfn "reduce %d" p
        let n = r p
        let r = pop p
        printfn "n %d" n
        printfn "r %d" r
        stack <- List.skip r stack
    
        match GoTo table 6 4 stack.Head n with
        | Goto g ->
            printfn "Goto %d" g
            stack <- g :: n :: stack    
            printfn "input: %A\nstack: %A" input stack
        | a -> printfn "goto error with %A" a
    
    | Actions.Accept -> printfn "Accept"
    | a -> printfn "reduce error with %A" a
    
    

type NT = T' | T | R
type Trm = A | B | C 

let test() =
    Productions [
        T' => [
            [&T] >>  fun _ -> printfn "T' -> T"
        ]
        T => [
            [&R] >>  fun _ -> printfn "T -> R"
            [!A; &T; !C] >>  fun _ -> printfn "T -> aTc"
        ]
        R => [
            [] >>  fun _ -> printfn "R -> "
            [!B; &R] >>  fun _ -> printfn "R -> bR"
        ]
    ]





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

(*
    let Action (table : 'a[]) state input = table.[state*6+input.GetHashCode()+1]
    let GoTo (table : 'a[]) state input = table.[state*6 + input.GetHashCode() + 3 ]
*)

// buffer the whole file for persistency
let Read path =
    let content = System.IO.File.ReadAllBytes path
    content.GetEnumerator() :> byte seq 

let test = Read "C:\Users\KAM\OneDrive\Skrivebord\Hello.txt"
let t' = Seq.tail test

for letter in test do printf "%c" (char letter)

let ret = Seq.skip 10 test |> Seq.head |> char