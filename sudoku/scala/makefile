.PHONY: clean all

all: SudokuSolver.class

run: SudokuSolver.class
	@echo "Running Sudoku Solver"
	scala SudokuSolver

SudokuSolver.class: Sudoku.scala
	@echo "Compiling Sudoku Solver"
	scalac Sudoku.scala -deprecation -feature -optimise

clean:
	rm -f *.class
