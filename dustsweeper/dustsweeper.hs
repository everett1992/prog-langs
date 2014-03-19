-- Read board size n from command line args
-- Read number of mines m from command line args
-- Generate n x n board  with m mines

-- Display the board                        <------+
-- Player inputs row (x) and column (y) from stdin |
-- Reveil space x,y in grid                        |
-- If it's a bomb the inactive player wins         |
-- Swap active and inactive players   -------------+

module Main (main) where

import System.Exit
import System.Environment
import Text.Read
import Data.Maybe
import Data.List

data Rug = Rug { is_explored :: Bool, dusts :: Int }

type Board = [[Rug]]


main = do
  args <- getArgs
  case parse_args args of
    Left a  -> exit_error a
    Right a -> print_board $ board (fst a) (snd a)

is_dust :: Rug -> Bool
is_dust rug
  | dusts rug == -1 = True
  | otherwise = False

-- Converts a Rug to a char
rug_char :: Rug -> Char
rug_char rug
  | (is_explored rug) = case (dusts rug) of num
                                              | num == (-1)       -> 'X'
                                              | num == 0           -> ' '
                                              | otherwise   -> head $ show num
  | otherwise = '░'

print_board board =
    putStrLn $ unlines $ map (\row -> map rug_char row) board

board :: Int -> Int -> Board
board size num_dusts = replicate size $ replicate size $ Rug False 0

-- Returns Either a tuple of (size, num_dusts) or an error string.
parse_args :: [String] -> Either String (Int,Int)
parse_args args
  | length args /= 2 = Left $ "Wrong number of args: " ++ show (length args) ++ " of 2"
  | otherwise = case (readMaybe $ args !! 0, readMaybe $ args !! 1) of
    (Nothing,_)      -> Left $ "Invalid Size was " ++ args !! 0 ++ " needs Int."
    (_,Nothing)      -> Left $ "Invalid number of dusts was "  ++ args !! 1 ++ " needs Int."
    (Just a, Just b) -> Right (a,b)

-- Print basic usage instructions
put_usage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " size:Int num_dusts:Int"

-- Print error msg, usage, and exit with a nonzero exit status
exit_error msg = do
  putStrLn msg
  put_usage
  exitFailure

