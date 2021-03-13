# Compiler
This is going to be a toy compiler for now with all stages implemented by my self.

When done it will be a general purpose concurrent reversible functional language. Reversible in the sense that if f is a function in program P then are the reverse function g also in P, hence f(g(x)) = g(f(x)) = x.

### OBS! See documentation below

## wokring
    * Lexer
    * Parser
    * Symbol Table Interface implemented, which ease code changes, and enable testing different scooping schemes depending on the symboltable 

## TODO
    * more testing on all parts
   
    * LR(k) for user defined k where the table is represented by a compressed table of an array of arrays.
    * rewriting campability of CFG to make the input grammar more easy to write
    * Implementaition of indentation sensitivity of the parser
   
    * Changing implementation of tokens and arguments in the lexer/parser to a two interface implementation for easier maintaince.
    
    * Type Checker
        - for the functional, imperative and intermidate language, based on SML type system.
        
    * Reversible constraint checker.

    * Optimizer (both in high and low level language)
    
    * Interpreter
    
    * Code generator which produce valid Rust code as output to be compiled, with focus on Rust bad compiler handling of tail recursion.
   
    
### when interpreter works
    * Intermediate language (2 kind, a typed and untyped)
    * Assembler (Arm is first focus)
    * Linker to avoid external software
    * Loader -||-


# Documentation

### disclaimers
As of now this is of version 0.0.1 (alpha) The Lexer is working, but is not optimised.
The parser will accept SLR Grammar only and it will make parse errors if you got some of the declared tokens missing in the productions when the token hare not defined as the last one. all tokens that should be filtered out afterward should be the last token in the list of tokens, since this will minimize the underlying DFA
If you try to decipher the type annotation of the input to the Lexer constructor you will get confused.
This will be handled later.

## Lexer
The 'Lexer' is a DFA based lexer interpreter, which in general are the same as a generator with the minor detail that it does not produces a file to be compiled, but produces a data structure that can be used directly inside FSharp interactive. which makes test based implementation easier.

To define a lexer you give it an array of string representation of Regex patterns. followed by the user defined token type and optional conversion function.

```Fsharp
let x = 0
```

