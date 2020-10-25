module Parser

(*

    First thoughs:

    The parser should take a list of rules, where a rule is a a list of patterns of lists of tokens/rules 
    We label each token type by a byte(or larger), then we label each rule by a number
    We then use this numbers to make a NFA over the rules, which we then convert to a DFA
    Followed by each rule should be a transformation function to apply to the rules return type of the form
    
        fun args : Args[] -> f (Take args.[i] ... Take args.[j])
        or (func,(i, ... ,j)) ? maybe add 'valueOf i' and 'PosOf j' functions to add readability to the parser

    This is needed because we can't just pass an arbitrary tuple with no prior information. (The array will most likely be optimized away anyway)

    After we got the normal parser to work we implement indentation sensitive parsing into it.

*)

