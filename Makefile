.PHONY: all test parser interpreter run build clean

SRC_DIRS := src src/lib
TEST_DIR := tests
GHC_FLAGS := $(foreach d, $(SRC_DIRS), -i$d) -package containers -package hspec

test:
	ghci $(GHC_FLAGS) -i$(TEST_DIR) $(TEST_DIR)/Main.hs -e main

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
	find . -name "*.o" -type f -delete
	find . -name "*.hi" -type f -delete
