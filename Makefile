Roo: Roo.hs Scanner.hs Parser.hs Pretty.hs AST.hs
	ghc Roo.hs

Scanner.hs: Scanner.x
	alex Scanner.x

Parser.hs: Parser.y
	happy -a -g -iparser.info Parser.y

clean:
	rm -f *.o *.hi
	rm -f Scanner.hs Parser.hs parser.info Roo


