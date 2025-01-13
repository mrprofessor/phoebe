.PHONY: all test parser interpreter run build clean

SRC_DIRS := src src/lib
GHC_FLAGS := $(foreach d, $(SRC_DIRS), -i$d) -package containers

test:
	ghci $(GHC_FLAGS) src/Tests.hs -e runTests

parser:
	ghci $(GHC_FLAGS) src/Parser.hs

interpreter:
	ghci $(GHC_FLAGS) src/Interpreter.hs

run:
	ghci $(GHC_FLAGS) src/Interpreter.hs

build:
	ghc $(GHC_FLAGS) -o dist/phoebe src/Main.hs

clean:
	rm -f dist/*

