OS=$(shell uname -s)
ifeq ($(OS),Darwin)
  export AS=as -arch i386
  export CC=cc -arch i386 -framework CoreFoundation -lobjc -liconv
endif

.PHONY: all clean 

fsc=fsharpc --nologo
fsi=fsharpi --nologo

#The executable
Compiler=bin/Compiler



#Interfaces
ILinker=bin/ILinker.dll
ILoader=bin/ILoader.dll
ISymbolTable=bin.dll

#libraries
Result=bin/Result.dll
Iter=bin/Iter.dll
Map=bin/Map.dll
Decoding=bin/Decoding.dll

Regex=bin/Regex.dll
DFA=bin/DFA.dll
Lexer=bin/Lexer.dll

Parser=bin/Parser.dll
AST=bin/AST.dll



all: $(Result) $(Decoding) $(Iter)

# Interface creation rules
$(ILinker): Interfaces/Linker.fsx
	$(fsc) -a Interfaces/Linker.fsx -o $(ILinker)

$(ILoader): Interfaces/Loader.fsx
	$(fsc) -a Interfaces/Loader.fsx -o $(ILoader)

$(ISymbolTable): Interfaces/SymbolTable.fsx
	$(fsc) -a Interfaces/SymbolTable.fsx -o $(ISymbolTable)


# Library creation rules
$(Result): src/Result.fsx
	$(fsc) -a src/Result.fsx -o $(Result)

$(Decoding): src/Decoding.fsx
  $(fsc) -a src/Decoding.fsx -o $(Decoding)

$(Iter): src/Iter.fsx $(Result) $(Decoding)
	$(fsc) -a src/Iter.fsx -r $(Result) -r $(Decoding) -o $(Iter)

$(AST): src/AST.fsx 
  $(fsc) -a src/AST.fsx -o $(AST) 

$(Map): src/Map.fsx $(Iter) $(Result)
  $(fsc) -a src/Map.fsx -o $(Map) -r $(Iter) -r $(Result)


$(Regex): src/Regex.fsx
  $(fsc) -a src/Regex.fsx -o $(Regex)

$(Lexer): src/Lexer.fsx
  $(fsc) -a src/Lexer.fsx -o $(Lexer)

$(DFA): src/DFA.fsx
  $(fsc) -a /src/DFA.fsx -0 $(DFA)


#Main program rule
$(Compiler): src/Compiler.fsx
  $(fsc) --nologo --standalone  src/Iter.fsx -o $(Compiler) 