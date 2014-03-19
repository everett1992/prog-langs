.PHONY: all clean run

all: dustsweeper

run: dustsweeper
	@echo "Running dustsweeper..."
	./dustsweeper 10 10

dustsweeper: dustsweeper.hs
	@echo "Compiling..."
	ghc dustsweeper.hs

clean:
	rm dustsweeper dustsweeper.hi dustsweeper.o

