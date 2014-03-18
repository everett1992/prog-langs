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

parseArgs :: [String] -> Either String (Int,Int)
parseArgs args
  | length args /= 2 = Left $ "Wrong number of args: " ++ show (length args) ++ " of 2"
  | otherwise = case (readMaybe $ args !! 0, readMaybe $ args !! 1) of
    (Nothing,_)      -> Left $ "Invalid Size was " ++ args !! 0 ++ " needs Int."
    (_,Nothing)      -> Left $ "Invalid number of bombs was "  ++ args !! 1 ++ " needs Int."
    (Just a, Just b) -> Right (a,b)

putUsage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " size:Int num_bombs:Int"

exitError msg = do
  putStrLn msg
  putUsage
  exitFailure


main = do
  args <- getArgs
  case parseArgs args of
    Left a  -> exitError a
    Right a -> print a
