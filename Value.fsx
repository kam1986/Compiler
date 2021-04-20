
/// this is an interface to convert a given token of some type into another type
/// implementing this for multiple types i.e.
///
/// ```
///     interface int Value with ....
///     interface float Value with ...
/// ```
/// will make it possible to store an intermidiate representation that can be cast to many types in the same collection
/// Here comes Tokens as use case in mind. A token is in general a struct like
/// 
/// ```
///     [<Struct>]
///     type Token =
///         val tok_type: int // cast a enum or D.U with no attributes to an int
///         val pos     : int * int * int // line, offset, absolutposition
///         val length  : int
/// ```
/// which can contain user definable non conformin types when implementing a lexer/parser. The tok_type is just a Id of a type instance
/// implementing 'a Value for the token with multiple possible output types make it possible to pass both int, float, bool or more complex types into a data structure without
/// knowing implementation details. In the example a token could be cast as an integer by slicing the context string 'content' from absolutpostion to length-1 and try parsing this as an integer.
/// the Result return type make error handling and reporting first class and the messaging user definable.
type 'a Value =
    abstract member cast : Result<'a, string>



let (!?) (value: _ Value) = 
    match value.cast with
    | Ok v -> v
    | Error msg ->
        printfn "%s" msg
        exit -1

type Arg =
    val value : string
    new (str) = {value = str}
    interface int Value with
        member A.cast = 
            try
                int A.value
                |> Ok
            with
            | err -> 
                "The value: '" + string A.value + "' was not an integer" 
                |> Error

    interface float Value with
        member A.cast = 
            try
                float A.value
                |> Ok
            with
            | err -> 
                "The value: '" + string A.value + "' was not an floating pointer number" 
                |> Error


let arg1 = Arg("1231")


type exp = I32 of int | F64 of float | Add of exp * exp
with
    static member (+) (e1, e2) = Add(e1, e2)

let rec eval =
    function
    | I32 i -> float i
    | F64 f -> f
    | Add (l,r) -> eval l + eval r

let args = [|Arg("132"); Arg("3.3"); Arg("4")|]

let ret = I32 !?args.[0] + F64 !?args.[1] + I32 !?args.[2]

eval ret