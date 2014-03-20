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


-- Prints the board
printBoard :: Board -> IO ()
printBoard board =
    putStrLn $ unlines $ map (\row -> map rugChar row) board


-- Creates a random s x s board with n dusts
board :: Int -> Int -> Board
board s n =
  setDusts (fst $ randPoints s n (mkStdGen 10)) (emptyBoard s)


-- Returns a board s x s board of unexploored non dust Rugs
emptyBoard :: Int -> Board
emptyBoard s = replicate s $ replicate s $ Rug False 0


-- Set the passed points in the board to dusts ( does not mark neighbors)
setDusts :: [Point] -> Board -> Board
setDusts points board =
  foldr (\p -> updateBoardAt p (\d -> d { dusts = (-1) })) board points


-- Call the function u with the rug at x, y, returning the 
-- board with the updated rug.
updateBoardAt :: Point -> (Rug -> Rug) -> Board -> Board
updateBoardAt p f b =
  updateAt (fst p) (updateAt (snd p) f) b


updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt n f xs =
  take n xs ++ [f (xs !! n)] ++ drop (n + 1) xs


-- Selects n random points from the s x s grid
--randPoints :: Int -> Int -> [Point]
randPoints s n = do
  sampleState (sample n $ points s) :: StdGen -> ([Point], StdGen)


points :: Int -> [Point]
points s = [ (a,b) | a <- [0..s-1], b <- [0..s-1] ]


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

