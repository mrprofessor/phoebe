test:
	ghci -isrc src/Tests.hs -package containers -e main

repl:
	ghci -isrc src/Interpreter.hs -package containers

