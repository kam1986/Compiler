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

Compiler=bin/Compiler.exe

all:  $(Compiler)

$(Iter): src/Iter.fs $(Decoding) $(Result)
	$(fsc) -a src/Iter.fs  -o $(Iter) -r $(Decoding) -r $(Result) 


$(Result): src/Result.fs
	$(fsc) -a src/Result.fs -o $(Result)

$(Decoding): src/Decoding.fs
	$(fsc) -a src/Decoding.fs -o $(Decoding)


$(Regex): src/Regex.fs $(Map) $(Iter) $(Result)
	$(fsc): -a src/Regex.fs -o $(Regex) -r $(Map) -r $(Iter) -r $(Result)

$(Map): src/Map.fs $(Result) $(Iter)
	$(fsc) -a src/Map.fs -o $(Map) -r $(Result) -r $(Iter)

$(DFA): src/DFA.fs $(Regex) $(Result) $(Iter) $(Map)
	$(fsc) -a src/DFA.fs -o $(DFA) -r $(Regex) -r $(Result) -r $(Iter) -r $(Map)

$(Compiler): src/Compiler.fsx $(Result) $(Iter) $(Map) $(Regex) $(DFA)
	$(fsc) src/Compiler.fsx -o $(Compiler) -r $(Result) -r $(Iter) -r $(Map) -r $(Regex) -r $(DFA)

clean:
	rm -rf bin/*