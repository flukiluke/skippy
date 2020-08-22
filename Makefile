Roo: Roo.hs Scanner.hs
	ghc Roo.hs

Scanner.hs: Scanner.x
	alex Scanner.x

clean:
	rm -f *.o *.hi
	rm -f Scanner.hs Roo


