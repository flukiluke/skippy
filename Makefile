Roo: Roo.hs Scanner.hs Parser.hs
	ghc Roo.hs

Scanner.hs: Scanner.x
	alex Scanner.x

Parser.hs: Parser.y
	happy -iparser.info Parser.y

clean:
	rm -f *.o *.hi
	rm -f Scanner.hs Parser.hs parser.info Roo


