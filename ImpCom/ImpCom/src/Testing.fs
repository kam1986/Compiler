
open DFA

let f (str : string) = 
    fun x -> 
        match x with
        | None -> "Error"
        | Some f -> f str

let ret =
    getAcceptancePrState states' acceptance
    |> Array.map (f "")


let states' = [set[-3;3;4;5;-1]; set[-5;2;3;4]; set[1;2]]
let acceptance = [|(fun (_ : string)-> "one"); (fun _ -> "two"); (fun _ -> "three"); (fun _ -> "four"); (fun _ -> "five")|]
// need the numbering of terminal to be in range -1 .. -n + 1 