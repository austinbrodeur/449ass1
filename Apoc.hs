{- |
Module      : Main
Description : Template to get you started on the CPSC 449 Winter 2016 Apocalypse assignment.
Copyright   : Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
License     : Permission to use, copy, modify, distribute and sell this software
              and its documentation for any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software for any purpose. It is provided "as is" without
              express or implied warranty.
Maintainer  : rkremer@ucalgary.ca
Stability   : experimental
Portability : ghc 7.10.2 - 7.10.3

This module is used for CPSC 449 for the Apocalypse assignment.

Feel free to modify this file as you see fit.

-}

module Apoc (
      -- * Main
      main, main', 
      
      --Game Functions
      intMode, getStrategy, checkModes, startGame,
     
      ) where

import System.Environment
import System.IO.Unsafe
import Data.Char
import Data.List
import Data.Maybe (fromJust, isNothing)
import ApocLoop
import ApocTools
import Util
import ApocStrategyHuman
import ApocStrategyOffensive
import ApocStrategyDefensive

---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)

main' :: [String] -> IO()
main' [] = do intMode
main' (x:y:xs) = do checkModes x y
main' xs = do putStrLn stratsPrint

-- Interactive mode. Shows strategies and gets user input.
intMode :: IO()
intMode = do
  putStrLn $ "\nEnter one of the strategies following strategies for each challenger:\n" ++ 
             "\nStrategies:\n  offensive\n  defensive\n  human\n"
  putStrLn "Enter the black strategy: "
  blackStrat <- getLine
  putStrLn "Enter the white strategy: "
  whiteStrat <- getLine
  checkModes blackStrat whiteStrat

--Gets the strategy from the String given.
getStrategy :: String -> Chooser
getStrategy name | (name == "human") = human
                   | (name == "offensive") = offensive
                   | (name == "defensive") = defensive

-- Checks if the input from either interactive mode or normal mode is correct and runs the game if it is.
checkModes :: String -> String -> IO()
checkModes blk wht = do
   let bs = getStrategy blk
   let ws = getStrategy wht
   if (elem (blk) stratsList) && (elem (wht) stratsList)
    then startGame bs blk ws wht
    else putStrLn $ "human\n  offensive\n  defensive\n"
  
-- Starts the game off.
startGame :: Chooser -> String -> Chooser -> String -> IO()
startGame bs blk ws wht = do
  print initBoard
  gameLoop initBoard bs blk ws wht
