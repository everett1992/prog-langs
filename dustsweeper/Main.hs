-- Read board size n from command line args
-- Read number of mines m from command line args
-- Generate n x n board  with m mines

-- Display the board                        <------+
-- Player inputs row (x) and column (y) from stdin |
-- Reveil space x,y in grid                        |
-- If it's a bomb the inactive player wins         |
-- Swap active and inactive players   -------------+

module Main (main) where

import Data.Random (sampleState)
import Data.Random.RVar
import Data.Random.Extras
import Data.Random.Source.StdGen
import System.Exit
import System.Environment
import Text.Read
import Data.Maybe
import Data.List

data Rug = Rug { isExplored :: Bool, dusts :: Int }

type Board = [[Rug]]
type Point = (Int,Int)
type Player = String


main = do
  args <- getArgs
  case parseBoardSize args of
    Left a  -> exitError a
    Right a -> newBoard (fst a) (snd a) >>= playGame ["Player 1", "Player 2"]

--playGame :: [Player] -> Int -> Int -> String
playGame players board = do
  printBoard $ board
  prompt players board

prompt players board = do
  putStrLn $ (head players) ++ "> "
  input <- getLine
  case parsePoint (words input) of
    Left a -> reprompt players board a
    Right a -> playGame (tail players ++ [head players]) (explore a board)

reprompt players board message = do
  putStrLn message
  prompt players board


-- Returns a new Board with the Rug at Point p in Board b explored
explore :: Point -> Board -> Board
explore p b = updateBoardAt p (\d -> d { isExplored = True }) b


-- Creates a random s x s board with n dusts
newBoard :: Int -> Int -> IO Board
newBoard s n = do
  gen <- newStdGen
  return $ setDusts (fst $ randPoints s n (gen)) (emptyBoard s)


-- Returns ture i the Rug r is a dust
isDust :: Rug -> Bool
isDust rug = dusts rug == (-1)


-- Returns a board size x size Board of unexplored non dust Rugs
emptyBoard :: Int -> Board
emptyBoard size = replicate size (replicate size (Rug False 0))


-- Set the passed points in the board to dusts (does not mark neighbors)
setDusts :: [Point] -> Board -> Board
setDusts points board =
  foldr (\p -> updateBoardAt p (\d -> d { dusts = (-1) })) board points


-- Return a new Board with the Rug r at point p  replaced by rug f(r)
updateBoardAt :: Point -> (Rug -> Rug) -> Board -> Board
updateBoardAt p f b =
  updateAt (fst p) (updateAt (snd p) f) b


-- Selects n random points from the s x s grid
randPoints :: Int -> Int -> (StdGen -> ([Point], StdGen))
randPoints s n = do
  let points = [ (a,b) | a <- [0..s-1], b <- [0..s-1] ]
  sampleState (sample n $ points) :: StdGen -> ([Point], StdGen)


-- Input Parseing Functions


-- Returns Either the inputed Point or an error string if input is invalid.
parsePoint :: [String] -> Either String Point
parsePoint input
  | length input /= 2 = Left $ "Wrong number of inputs, needs X Y"
  | otherwise = case (readMaybe $ input !! 0, readMaybe $ input !! 1) of
    (Nothing,_)      -> Left $ "X should to be an Int, but was '" ++ input !! 0 ++ "'."
    (_,Nothing)      -> Left $ "Y should to be an Int, but was '" ++ input !! 1 ++ "'."
    (Just a, Just b) -> Right (a,b)


-- Returns Either a tuple of (size, numDusts) or an error string.
parseBoardSize :: [String] -> Either String (Int, Int)
parseBoardSize args
  | length args /= 2 = Left $ "Wrong number of args: " ++ show (length args) ++ " of 2"
  | otherwise = case (readMaybe $ args !! 0, readMaybe $ args !! 1) of
    (Nothing,_)      -> Left $ "Invalid Size was " ++ args !! 0 ++ " needs Int."
    (_,Nothing)      -> Left $ "Invalid number of dusts was "  ++ args !! 1 ++ " needs Int."
    (Just a, Just b) -> Right (a,b)


-- Print Functions


-- Print basic usage instructions
putUsage :: IO ()
putUsage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " size:Int numDusts:Int"


-- Print error msg, usage, and exit with a nonzero exit status
exitError msg = do
  putStrLn msg
  putUsage
  exitFailure

-- Converts a Rug to a char
rugChar :: Rug -> Char
rugChar rug
  | (isExplored rug) = case (dusts rug) of num
                                            | num == (-1) -> 'X'
                                            | num == 0    -> ' '
                                            | otherwise   -> head $ show num
  | otherwise = '░'


-- Prints the board
printBoard :: Board -> IO ()
printBoard board =
    putStrLn $ unlines $ map (\row -> map rugChar row) board


-- Utility Functions

-- Return a new list with the value at n replaced with the value f(n)
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt n f xs =
  take n xs ++ [f (xs !! n)] ++ drop (n + 1) xs


