.PHONY: all test build clean repl parser interpreter 

micro:
	ghci -isrc src/micro.hs -package containers -e repl

test:
	ghci -isrc src/Tests.hs -package containers -e main

build:
	ghc -isrc -main-is Repl src/Repl.hs -o bin/repl -package containers

clean:
	rm -f bin/*

repl:
	ghci -isrc src/Repl.hs -package containers -e main

parser:
	ghci -isrc src/Parser.hs -package containers

interpreter:
	ghci -isrc src/Interpreter.hs -package containers

