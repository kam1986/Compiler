# Compiler
This is going to be a toy compiler for now with all stages implemented by my self.

When done it will be a general purpose Reversible functional language. Reversible in the sense that if f is a function in program P then are the reverse function g also in P, hence f(g(x)) = g(f(x)) = x.



## wokring
    * Lexer
        - Own lexer generator writen in F#, DFA based in contrast to FSyacc which use System.Text.Regex that is based on NFA. This should increase speed but not in any case near yacc for C. No code generation step needed. 
    * Parser
        - Own parser generator writen in F#, should again be a DFA/stack driven LR Parser generator. again no code generation step needed and should be at least as fast as Fsyacc.
    
    * Symbol Table
        - Interface implemented, which ease code changes, and enable testing different scooping schemes depending on the symboltable 

## TODO
    * Changing implementation of tokens and arguments in the lexer/parser to an two interface implementation for easier maintaince.
    
    * Type Checker
        - for the functional, imperative and intermidate language, based on SML type system.

    * Optimizer (both in high and low level language)

    * Intermediate language (2 kind, a typed and untyped)
    * Assembler (Arm is first focus)
    * Linker to avoid external software
    * Loader -||-
