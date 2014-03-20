.PHONY: all clean run

all: Dustsweeper

run: Dustsweeper
	@echo "Running Dustsweeper..."
	./Dustsweeper 10 10

Dustsweeper: Dustsweeper.hs
	@echo "Compiling..."
	ghc Dustsweeper.hs

clean:
	rm Dustsweeper Dustsweeper.hi Dustsweeper.o

