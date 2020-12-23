module SymbolTable

open Position

// interface because it makes it easy to change implementation without changing the other code.
// and if we use an interpreter it might be handy to could switch between types of tables depending
// on size of the table. i.e. if we have a small number of variable then a list representation is fast enough
// on the other hand do we have a case where hundred to millions are named then a Map representation would be faster
// and with the interface we could implement the intepreter to switch between different types of table to handle this.

/// The usage of this interface allow use to change implementation specifics easy without needing to change other code.
/// It specifies what to expect, and allow for interchangeable types of table when running an intepreter.
/// it also lets the user switch between static scooping and dynamic scooping to see the effect, or handle specific
/// nesting constrains in respect to how binding rules are.
type 'S ISymBolTable =
    abstract LookUp : string -> Result<'S, string>
    abstract Bind : string -> 'S -> 'S ISymBolTable
    abstract Remove : string -> 'S ISymBolTable
    abstract Union : 'S ISymBolTable -> 'S ISymBolTable
    abstract OfSeq : (string * 'S) seq -> 'S ISymBolTable
    abstract ToSeq : (string * 'S) seq 




type 'S SymbolList = 
    List of (string * 'S) list
with interface ISymBolTable<'S> with        
       
        member L.LookUp item =
            let (List lst) =  L
            match lst with
            | [] -> sprintf "The item : %s was not found" item |> Error
            | (name, ret) :: _ when name = item -> Ok ret
            | _ :: lst -> (List lst :> ISymBolTable<'S>).LookUp item

        member L.Bind name item =
            let (List lst) = L
            List ((name,item) :: lst) :> ISymBolTable<'S>
            

        member L.Remove name =
            let rec remove item acc lst =
                match lst with
                | [] -> List.rev acc |> List
                // remove the first match found
                | (item, _) :: lst when item = name -> 
                    List.fold (fun acc a -> a :: acc) lst acc |> List
                | l :: lst -> remove item (l :: acc) lst
            let (List lst) = L
            remove name [] lst :> 'S ISymBolTable


        member L.Union (tab : _ ISymBolTable) =
            let (List lst) = L
            // inserting the bindings of tab into L overshadowing bindings of L
            (List (Seq.foldBack (fun s lst -> s :: lst) tab.ToSeq lst)) :> 'S ISymBolTable
                    
        member L.OfSeq s = Seq.toList s |> List :> 'S ISymBolTable

        member L.ToSeq =
            let (List lst) = L
            List.toSeq lst


type 'S SymbolMap =
    Table of Map<string,'S> with 
    interface 'S ISymBolTable with
    
       member L.LookUp item =
            let (Table tab) = L
            match Map.tryFind item tab with
            | None -> sprintf "The item : %s was not found" item |> Error
            | Some s -> Ok s

        member L.Bind name item =
            let (Table tab) = L
            Table (Map.add name item tab) :> 'S ISymBolTable

        member L.Remove name =
            let (Table tab) = L
            Table (Map.remove name tab) :> 'S ISymBolTable

        member L.Union (tab : _ ISymBolTable) =
            let (Table tab') = L
            Table (Seq.foldBack (fun (name, item) tab -> Map.add name item tab) tab.ToSeq tab') :> 'S ISymBolTable

        member L.ToSeq =
            let (Table tab) = L
            Map.toSeq tab

        member L.OfSeq s =
           Table (Map.ofSeq s) :> 'S ISymBolTable


/// Functional style call to a ISymboltable of lookup
let LookUp item (table : 'S ISymBolTable) = table.LookUp item

/// Functional style call to a ISymboltable of bind
let Bind name item (table : 'S ISymBolTable) = table.Bind name item

/// Functional style call to a ISymboltable of Remove
let Remove name (table: 'S ISymBolTable) = table.Remove name

/// Functional style call to a ISymboltable of union, here tab1 overshadow any doublicate bindings of tab2
/// This can be used to convert between to types of ISymboltable.
///
/// let tab1 be a none empty intans of list implementation as the above 
/// let tab2 of some other implemenation of ISymboltable, then
///
/// let tab3 = Union tab2.Empty tab1
///
/// would be an ISymbolTable with the same bindings as tab1 but of the implementation type tab2
let Union (tab1 : 'S ISymBolTable) (tab2 : 'S ISymBolTable) = tab2.Union tab1


