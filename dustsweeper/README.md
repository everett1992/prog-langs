% Caleb Everett
% Programming languages - Haskell dustsweeper.

# Dustsweeper

A terminal minesweeper clone written in Haskell.

## Whats working:
* Board is generated with bombs
* Hints are calculated
* Players take turns reveiling Rugs

## Whats not working:
* No win condition


## Complilation

Compile Main.hs with the included makefile by running

```
$ make
```

or using ghc with

```
$ ghc Main.hs
```

## Usage

Run the program with `$ make run `, or to play with a different sized board `$ ./Main s n` where
`s` is the width of the square board, and `n` is the number of bombs.


