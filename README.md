# Compiler
This is going to be a toy compiler for now with all stages implemented by my self.

When done it will be a general purpose concurrent reversible functional language. Reversible in the sense that if f is a function in program P then are the reverse function g also in P, hence f(g(x)) = g(f(x)) = x.



## wokring
    * Lexer
    * Parser
    * Symbol Table Interface implemented, which ease code changes, and enable testing different scooping schemes depending on the symboltable 

## TODO
    * Changing implementation of tokens and arguments in the lexer/parser to an two interface implementation for easier maintaince.
    
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
