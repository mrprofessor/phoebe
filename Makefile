.PHONY: all test build clean parser interpreter 

test:
	ghci -isrc src/Tests.hs -package containers -e main

build:
	ghc -isrc -main-is Repl src/Repl.hs -o bin/repl -package containers

clean:
	rm -f bin/*


parser:
	ghci -isrc src/AST.hs -package containers

interpreter:
	ghci -isrc src/Interpreter.hs -package containers

