module TypeAcrobatics

// just a placeholder
type Type = Type of obj

let inline AnyType t = Type (t :> obj)

// carry argument as a postpond transformation
// i.e. the type checking is first done, at run time. 
type Arg = Arg of (unit -> Type)


let inline Delay work input = 
    fun _ -> work input |> AnyType
    |> Arg 

let inline Take (Arg a) = 
    let (Type t) = a()
    t :?> _ // this will be infered by where we put the value



type TYPES = INT of int | FLOAT of float | ERROR

// a way to fuck with the type system :D 
let GetValue (Type t) = t :?> _

// This  enable type conversion when tranforming without specific type parameters given

let toFloat (str : string) = System.Double.Parse str
let toInt (str : string) = System.Int32.Parse str

let d = Delay toFloat "0.113"
let i = Delay toInt "123"

let ret3 = FLOAT <| GetValue (Take d)
let ret4 = INT <| GetValue (Take i)