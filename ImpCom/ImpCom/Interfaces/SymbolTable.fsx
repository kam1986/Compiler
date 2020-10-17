module SymbolTable

(*

    Generic Symboltable Interface.
    This leads to easy to implement and change symboltable implementation without bracking the code.

*)



#r "../bin/Result.dll"
open result

type ISymbolTable =
    abstract member LookUp : 'name -> 'item Result
    abstract member Bind : 'name -> 'item -> ISymbolTable
    abstract member Union : ISymbolTable -> ISymbolTable
    abstract member Unbind : 'name -> ISymbolTable

// more functional like functions
let Bind key item (symbtab : ISymbolTable) = symbtab.Bind key item
let LookUp key (symbtab : ISymbolTable) = symbtab.LookUp key
let Unbind key (symbtab : ISymbolTable) = symbtab.Unbind key
let Union (symbtab1 : ISymbolTable) symbtab2 = symbtab1.Union symbtab2