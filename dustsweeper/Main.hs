-- Read board size n from command line args
-- Read number of mines m from command line args
-- Generate n x n board  with m mines

-- Display the board                        <------+
-- Player inputs row (x) and column (y) from stdin |
-- Reveil space x,y in grid                        |
-- If it's a bomb the inactive player wins         |
-- Swap active and inactive players   -------------+

module Main (main) where

import Data.Random.RVar
import Data.Random.Extras
import Data.Random.Source.DevRandom
import System.Exit
import System.Environment
import Text.Read
import Data.Maybe
import Data.List

data Rug = Rug { isExplored :: Bool, dusts :: Int }

type Board = [[Rug]]


main = do
  args <- getArgs
  case parseArgs args of
    Left a  -> exitError a
    Right a -> printBoard $ board (fst a) (snd a)

isDust :: Rug -> Bool
isDust rug
  | dusts rug == -1 = True
  | otherwise = False

-- Converts a Rug to a char
rugChar :: Rug -> Char
rugChar rug
  | (isExplored rug) = case (dusts rug) of num
                                              | num == (-1)       -> 'X'
                                              | num == 0           -> ' '
                                              | otherwise   -> head $ show num
  | otherwise = '░'

printBoard board =
    putStrLn $ unlines $ map (\row -> map rugChar row) board

board :: Int -> Int -> Board
board size numDusts =
  emptyBoard size

emptyBoard :: Int -> Board
emptyBoard size = replicate size $ replicate size $ Rug False 0

-- randLocs :: Int -> Int -> [(Int, Int)]
randLocs size numDusts = do
  runRVar (sample numDusts $ points size) DevRandom

points :: Int -> [(Int, Int)]
points size =
   [ (a,b) | a <- [0..size-1], b <- [0..size-1] ]

-- Returns Either a tuple of (size, numDusts) or an error string.
parseArgs :: [String] -> Either String (Int,Int)
parseArgs args
  | length args /= 2 = Left $ "Wrong number of args: " ++ show (length args) ++ " of 2"
  | otherwise = case (readMaybe $ args !! 0, readMaybe $ args !! 1) of
    (Nothing,_)      -> Left $ "Invalid Size was " ++ args !! 0 ++ " needs Int."
    (_,Nothing)      -> Left $ "Invalid number of dusts was "  ++ args !! 1 ++ " needs Int."
    (Just a, Just b) -> Right (a,b)

-- Print basic usage instructions
putUsage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " size:Int numDusts:Int"

-- Print error msg, usage, and exit with a nonzero exit status
exitError msg = do
  putStrLn msg
  putUsage
  exitFailure

