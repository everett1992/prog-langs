.PHONY: all clean run

all: Main

run: Main
	@echo "Running Main..."
	./Main 10 10

Main: Main.hs
	@echo "Compiling..."
	ghc Main.hs

clean:
	rm Main Main.hi Main.o

