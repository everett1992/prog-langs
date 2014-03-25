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
import Data.Tuple

-- The basic rug type
data Rug = Rug { isExplored :: Bool, isDust :: Bool, hint :: Int}

-- State of a board.
data State = Won | Lost | Ongoing

-- A board is a list of lists of Rugs.
type Board = [[Rug]]
type Point = (Int,Int)
-- The player type is an abstration of a string representing their name.
type Player = String
type Players = (Player, Player)

type PointError = Either String Point

-- Extracted here for debugging, change this value to change the default
-- Rugs in a Board.
emptyRug = Rug False False 0


main = do
  args <- getArgs
  gen <- getStdGen
  case parseBoardSize args of
    Left a  -> exitError a
    Right (s,n) -> playGame players (newBoard s n gen) >>= announceWinner
  where
    players = ("Player 1", "Player 2")


announceWinner :: (Player, Board) -> IO ()
announceWinner (winner, board) = do
  printBoard board
  putStrLn $ winner ++ " Wins!"

-- Type not finalized, I don't know the final return type yet.
playGame :: Players -> Board -> IO (Player, Board)
playGame players board = do
  printBoard $ board
  point <- prompt (fst players) board
  let nextBoard = (explore point board)
  case state nextBoard of
    Won  -> return ((fst players), nextBoard)
    Lost -> return ((snd players), nextBoard)
    Ongoing -> playGame (swap players) (floodExploredRugs nextBoard)


-- The state of the passed board
state :: Board -> State
state board
  | length (unexploredEmptyRugs rugs) == 0  = Won
  | length (exploredDusts rugs) > 0         = Lost
  | otherwise                               = Ongoing
  where rugs = concat board


exploredDusts :: [Rug] -> [Rug]
exploredDusts board =
  filter (\a -> (isExplored a) && (isDust a)) board

unexploredEmptyRugs :: [Rug] -> [Rug]
unexploredEmptyRugs board =
  filter (\a -> not (isExplored a) && not (isDust a)) board


-- Ask for input, if it's invalid ask again,
-- if it's valid return an IO Point.
prompt :: Player -> Board -> IO Point
prompt player board = do
  putStr $ player ++ "> "
  hFlush stdout -- Ensure putStr is outputted
  input <- getLine
  case parsePoint board (words input) of
    Left a -> reprompt player board a
    Right a -> return $ a

-- Print an error message then ask for input again.
reprompt :: Player -> Board -> String -> IO Point
reprompt player board message = do
  putStrLn message
  prompt player board


-- Returns a new Board with the Rug at Point p in Board b explored
explore :: Point -> Board -> Board
explore p b = nb
  where
   nb = updateBoardAt p (\d -> d { isExplored = True }) b

floodExploredRugs :: Board -> Board
floodExploredRugs b = case length (unexploredAdjacentNonDusts b) of
  0 -> b
  otherwise -> floodExploredRugs $ foldr explore b (unexploredAdjacentNonDusts b)


-- List of all non dust, non explored points next to explored points.
unexploredAdjacentNonDusts :: Board -> [Point]
unexploredAdjacentNonDusts b = filter
  (\p -> (not (isExplored (rugAt b p)) && not (isDust (rugAt b p))))
  (concat $ map (adjPoints b) (exploredNonHintPonts b))


exploredNonHintPonts :: Board -> [Point]
exploredNonHintPonts b = filter
  (\p -> hint (rugAt b p) == 0)
  (exploredPoints b)

-- The Points of all explored rugs in the Board b
exploredPoints :: Board -> [Point]
exploredPoints b = filter
  (\p -> isExplored (rugAt b p))
  (points (length b))


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


-- Increments the number of dusts in all Rugs adjacent Points
setHints :: [Point] -> Board -> Board
setHints points board = foldr incDusts board incPoints
  where
    incDusts = \p -> updateBoardAt p (\d -> d { hint = succ (hint d) })
    incPoints = concat $ map (adjPoints board) points


-- Return a new Board with the Rug r at point p  replaced by rug f(r)
updateBoardAt :: Point -> (Rug -> Rug) -> Board -> Board
updateBoardAt p f b =
  updateAt (fst p) (updateAt (snd p) f) b

-- The Rug from a Board at point `x`, `y`
rugAt :: Board -> Point -> Rug
rugAt board (x,y) = (board !! x) !! y

-- List of points adjacent to the Point.
adjPoints :: Board -> Point -> [Point]
adjPoints b (x,y) = [(x',y') |
   x' <- [x-1..x+1],
   y' <- [y-1..y+1],
   x' /= x || y' /= y,
   x' >= 0, x' < length b,
   y' >= 0, y' < length b  ]


-- List of all points in `s` by `s` grid
points :: Int -> [Point]
points s = [ (x,y) | x <- [0..(s-1)], y <- [0..(s-1)] ]

-- List of `n` random points inside a `s` by `s` grid
randPoints :: RandomGen g => Int -> Int -> g -> [Point]
randPoints s n g = take n $ nub $ zip xs ys
    where
      xs = filter (\a -> a `mod` 2 == 0) (randomRs (0, s-1) g)
      ys = filter (\a -> a `mod` 2 /= 0) (randomRs (0, s-1) g)


-- Input Parseing Functions


-- Error message to display when input cannot be parsed as Int.
readErrorMsg :: String -> String -> String
readErrorMsg var input = "Error parsing input: " ++ var
   ++ " should to be an Int, but was '" ++ input ++ "'."

-- Error message to display when input is out of bounds.
boundsErrorMsg :: String -> Int -> Int -> Int -> String
boundsErrorMsg var min max is = "Invalid move: " ++ var
  ++ " must be within " ++ (show min) ++ " and " ++ (show max)
   ++ " but was " ++ (show is) ++ "."

exploredErrorMsg :: Point -> String
exploredErrorMsg point = "Invalid move: " ++ show point
  ++ ". Rug is already explored."


-- Returns Either the inputed Point or an error string if input is invalid.
parsePoint :: Board -> [String] -> PointError
parsePoint board input
  | length input /= 2 = Left $ "Wrong number of inputs, needs X Y"
  | otherwise = case (readMaybe $ input !! 0, readMaybe $ input !! 1) of
    (Nothing,_)      -> Left $ readErrorMsg "X" (input !! 0)
    (_,Nothing)      -> Left $ readErrorMsg "Y" (input !! 1)
    (Just x, Just y) -> validPoint board (x,y)


-- Checks that the move is unexplored, and within the bounds of the board.
validPoint :: Board -> Point -> PointError
validPoint board point = case checkBounds 0 (length board - 1) point of
  Left a  -> Left a
  Right a -> unexplored board a

-- Checks that the move is unexplored.
unexplored :: Board -> Point -> PointError
unexplored board point
  | isExplored (rugAt board point) = Left $ exploredErrorMsg point
  | otherwise = Right point


-- Checks that the move is with in the bounds of the board.
checkBounds :: Int -> Int -> Point -> PointError
checkBounds min max (x,y)
  | x < min || x > max   = Left $ boundsErrorMsg "X" min max x
  | y < min || y > max   = Left $ boundsErrorMsg "Y" min max y
  | otherwise                    = Right (x,y)


argLengthErrorMsg :: Int -> Int -> String
argLengthErrorMsg needs is = "Wrong number of args: " ++ show is 
  ++ " of " ++ show needs ++ "."

intErrorMsg :: String -> String -> String
intErrorMsg var is =
  "Invalid " ++ var ++ " '"  ++ is ++ "' must be an Int."

-- Returns Either a tuple of (size, numDusts) or an error string.
parseBoardSize :: [String] -> Either String (Int, Int)
parseBoardSize args
  | length args /= 2 = Left $ argLengthErrorMsg 2 (length args)
  | otherwise = case (readMaybe $ args !! 0, readMaybe $ args !! 1) of
    (Nothing,_)      -> Left $ intErrorMsg "size" (args !! 0)
    (_,Nothing)      -> Left $ intErrorMsg "number of dusts " (args !! 0)
    (Just a, Just b) -> Right (a,b)


-- Print Functions


-- Print basic usage instructions
putUsage :: IO ()
putUsage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " size numDusts"


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
