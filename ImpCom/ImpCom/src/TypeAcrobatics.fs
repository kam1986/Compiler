module TypeAcrobatics

/// Abstraction that hide the type from the type system.
/// enabling collection of none uniform types
type Type = Type of obj

let inline AnyType t = Type (t :> obj)

// carry argument as a postpond transformation
// i.e. the type checking is first done, at run time. 
type token = Arg of (unit -> Type)


let inline Delay work input = 
    fun _ -> work input |> AnyType
    |> Arg 

let Arg item = Delay id item


/// This safely cast the the type
/// will throw an error if the return value are not use properly
/// example:
///     let One = Arg 1
///     printfn "%d" <| Take One // here the expected input of printfn is int do To '%d' it also do fine with %A
let inline Take (Arg a) = 
    let (Type t) = a()
    t :?> _ 


let t = [Arg (printfn "first"); Arg 1; Arg 2.3]