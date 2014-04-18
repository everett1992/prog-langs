% Caleb Everett
% Programming languages - Haskell dustsweeper.

# Dustsweeper

A terminal minesweeper clone written in Haskell.

## Whats working:
* Everything nessesary.

## Whats not working:
* Dusts are calculated when the program starts, so it is possible
for a player to loose on their first turn.


## Complilation

Compile Dustsweeper.hs with the included makefile by running

```
$ make
```

or using ghc with

```
$ ghc Dustsweeper.hs
```

## Usage

Run the program with `$ make run `, or to play with a different sized board `$ ./Dustsweeper s n` where
`s` is the width of the square board, and `n` is the number of bombs.
