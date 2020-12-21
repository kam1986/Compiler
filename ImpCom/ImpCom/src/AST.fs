module AST

open Token
(*
    A syntax tree is a multi-ary tree where each node is either a node or a leaf. 

    The below code is highly abstract and we could make a specific node of each terminal and
    non terminal.

    both tag and tp (type) are enums because it will enhance performance, and ease the implementation
    of none primitive types without altering the code, and to simply ignore types before type checking.

*)
type SyntaxTree<'e, 't, 'tp, 'op,'value when 'op: enum<'e> and 'tp : enum<'t>> =
    | Leaf of tag : 'op * field : 'value * Position 
    | Node of tag : 'op * fields : SyntaxTree<'e, 't, 'tp, 'op,'value>[] * Position
