namespace Testing

open System
open Microsoft.VisualStudio.TestTools.UnitTesting


open SymbolTable


[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestMethodPassing () =
        printfn "Testing symbol table implementations"
        let t = List [] :> _ ISymBolTable
        let table = [("Kasper", 34); ("Nadia", 25); ("Regitze", 7); "Angela", 5; "Gilert",2 ]
        
        // constructing table
        let tab = List.fold (fun tab (name, item) -> Bind name item tab) t table
        // testing for valid name
        List.fold (fun _ (name, age) ->  Assert.AreEqual(LookUp name tab, Ok age)) () table 
        // testing lookup on invalid name
        Assert.AreEqual(LookUp "NoBody" tab,  Error "The item : NoBody was not found")
        ( // testing for overshadowing
            let tab = Bind "Kasper" 35 tab
            Assert.AreEqual(LookUp "kasper" tab, Ok 35)
        )
        // testing scoping
        Assert.AreEqual(LookUp "kasper" tab, Ok 34)

        // transform to map based type this cover testing for bind too.
        let tab = 
            let t = Table Map.empty
            Union tab t

        List.fold (fun _ (name, age) ->  Assert.AreEqual(LookUp name tab, Ok age)) () table 
        printfn "Success"

        