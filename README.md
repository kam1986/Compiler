# Compiler
This is going to be a toy compiler for now with all stages implemented by my self.


## TODO
    * Lexer
        - Own lexer generator writen in F#, DFA based in contrast to FSyacc which use System.Text.Regex that is based on NFA. This should increase speed but not in any case near yacc for C. No code generation step needed. 
    * Parser
        - Own parser generator writen in F#, should again be a DFA/stack driven LR Parser generator. again no code generation step needed and should be at least as fast as Fsyacc.
    * Abstract Syntax Tree
        - for both the Imperative and the Functional paradigm, and reversible
    * Symbol Table
        - Interface implemented, which ease code changes, and enable testing different scooping schemes depending on the symboltable 
    * Type Checker
        - for the functional, imperative and intermidate language, based on SML type system.

    * Optimizer (both in high and low level language)

    * Intermediate language (2 kind, a typed and untyped)
    * Assembler (Arm is first focus)
    * Linker to avoid external software
    * Loader -||-