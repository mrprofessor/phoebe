.PHONY: all test parser interpreter clean

test:
	ghci -isrc src/Tests.hs -package containers -e runTests

parser:
	ghci -isrc src/AST.hs -package containers

interpreter:
	ghci -isrc src/Interpreter.hs -package containers

clean:
	rm -f bin/*

