.PHONY: all test build clean repl interpreter

test:
	ghci -isrc src/Tests.hs -package containers -e main

build:
	ghc -isrc -main-is Repl src/Repl.hs -o bin/repl -package containers

clean:
	rm -f bin/*

repl:
	ghci -isrc src/Repl.hs -package containers -e main

interpreter:
	ghci -isrc src/Interpreter.hs -package containers
