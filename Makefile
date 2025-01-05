.PHONY: all test parser interpreter run build clean

test:
	ghci -isrc src/Tests.hs -package containers -e runTests

parser:
	ghci -isrc src/Parser.hs -package containers

interpreter:
	ghci -isrc src/Interpreter.hs -package containers

run:
	ghci -isrc src/Interpreter.hs -package containers

build:
	ghc -o bin/interpreter src/Interpreter.hs -package containers

clean:
	rm -f bin/*

