# Skippy, a compiler for the Roo language.
#
# Submitted for assignment 1b of COMP90045, 2020
# By Luke Ceddia [lceddia] and Ben Harper [bharper1]
# 16 September 2020
#
# This program is licensed under the MIT license; see the LICENCE file for
# full details.
#
# Basic makefile. Needs ghc, alex and happy installed.

Roo: Roo.hs Scanner.hs Parser.hs Pretty.hs AST.hs SymbolTable.hs CodeGen.hs
	ghc Roo.hs

Scanner.hs: Scanner.x
	alex Scanner.x

Parser.hs: Parser.y
	happy -c -a -g -iparser.info Parser.y

clean:
	rm -f *.o *.hi
	rm -f Scanner.hs Parser.hs parser.info Roo


