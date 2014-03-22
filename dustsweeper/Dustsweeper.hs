{- Programming Languages                                      C. Everett -}
{- Assignment 4                                                     TCNJ -}
{- Haskell minesweeper clone                                             -}
{- Dependancies: none                                                    -}
module Main (main) where

import System.IO
import System.Exit
import System.Environment
import System.Random
import Text.Read
import Data.Maybe
import Data.List

-- The basic rug type
data Rug = Rug { isExplored :: Bool, isDust :: Bool, hint :: Int}

-- A board is a list of lists of Rugs.
type Board = [[Rug]]
type Point = (Int,Int)
-- The player type is an abstration of a string representing their name.
type Player = String

-- Extracted here for debugging, change this value to change the default
-- Rugs in a Board.
emptyRug = Rug False False 0


main = do
  args <- getArgs
  gen <- getStdGen
  case parseBoardSize args of
    Left a  -> exitError a
    Right (s,n) -> playGame ["Player 1", "Player 2"] (newBoard s n gen)

-- Type not finalized
playGame players board = do
  printBoard $ board
  prompt players board

-- Type not finalized
prompt players board = do
  putStr $ (head players) ++ "> "
  hFlush stdout -- Ensure putStr is outputted
  input <- getLine
  case parsePoint board (words input) of
    Left a -> reprompt players board a
    Right a -> playGame (tail players ++ [head players]) (explore a board)

-- Type not finalized
reprompt players board message = do
  putStrLn message
  prompt players board


-- Returns a new Board with the Rug at Point p in Board b explored
explore :: Point -> Board -> Board
explore p b = updateBoardAt p (\d -> d { isExplored = True }) b


-- Creates a random s by s board with n dusts
newBoard :: Int -> Int -> StdGen -> Board
newBoard s n g = setHints points $ setDusts points (emptyBoard s)
    where points = randPoints s n g


-- Returns a size by size Board of unexplored non dust Rugs
emptyBoard :: Int -> Board
emptyBoard size = replicate size (replicate size emptyRug)


-- Set the passed points in the board to dusts (does not mark neighbors)
setDusts :: [Point] -> Board -> Board
setDusts points board =
  foldr (\p -> updateBoardAt p (\d -> d { isDust = True })) board points


-- Increments the number of dusts in all Rugs neighboring Points
setHints :: [Point] -> Board -> Board
setHints points board = foldr incDusts board incPoints
  where
    incDusts = \p -> updateBoardAt p (\d -> d { hint = succ (hint d) })
    incPoints = concat $ map (neighboringPoints board) points


-- Return a new Board with the Rug r at point p  replaced by rug f(r)
updateBoardAt :: Point -> (Rug -> Rug) -> Board -> Board
updateBoardAt p f b =
  updateAt (fst p) (updateAt (snd p) f) b

rugAt :: Board -> Int -> Int -> Rug
rugAt board x y = (board !! x) !! y

neighboringPoints :: Board -> Point -> [Point]
neighboringPoints b (x,y) = [(x',y') | x' <- [x-1..x+1], y' <- [y-1..y+1], x' /= x || y' /= y, x' >= 0, x' < length b, y' >= 0, y' < length b  ]


randPoints :: RandomGen g => Int -> Int -> g -> [Point]
randPoints s n g = take n $ nub $ zip xs ys
    where
      xs = filter (\a -> a `mod` 2 == 0) (randomRs (0, s-1) g)
      ys = filter (\a -> a `mod` 2 /= 0) (randomRs (0, s-1) g)


-- Input Parseing Functions


-- Returns Either the inputed Point or an error string if input is invalid.
parsePoint :: Board -> [String] -> Either String Point
parsePoint board input
  | length input /= 2 = Left $ "Wrong number of inputs, needs X Y"
  | otherwise = case (readMaybe $ input !! 0, readMaybe $ input !! 1) of
    (Nothing,_)      -> Left $ "X should to be an Int, but was '" ++ input !! 0 ++ "'."
    (_,Nothing)      -> Left $ "Y should to be an Int, but was '" ++ input !! 1 ++ "'."
    (Just x, Just y) -> case (x,y) of point
                                        | x < 0 || x >= length board   -> Left $ "X must be within 0 and " ++ show (length board - 1) ++ " but was " ++ show x ++ "."
                                        | y < 0 || y >= length board   -> Left $ "Y must be within 0 and " ++ show (length board - 1) ++ " but was " ++ show y ++ "."
                                        | isExplored (rugAt board x y) -> Left $ "Rug at (" ++ show x ++ ", " ++ show y ++ ") is already explored."
                                        | otherwise                    -> Right (x,y)


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
  | isExplored rug && isDust rug  = 'X'
  | isExplored rug && not (isDust rug) = hintChar $ hint rug
  | otherwise = '░'

hintChar :: Int -> Char
hintChar hint
  | hint == 0 = ' '
  | otherwise = head $ show hint



-- Prints the board
printBoard :: Board -> IO ()
printBoard board =
    putStrLn $ unlines $ map (map rugChar) board
-- Utility Functions

-- Return a new list with the value at n replaced with the value f(n)
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt n f xs =
  take n xs ++ [f (xs !! n)] ++ drop (n + 1) xs
