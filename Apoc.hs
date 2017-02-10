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

module Main (
      -- * Main
      main, main',
      -- * Utility functions
      replace, replace2
      ) where

import Data.Maybe (fromJust, isNothing)
import System.Environment
import System.IO.Unsafe
import ApocTools
import ApocStrategyHuman
import Data.List.Split


---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:

     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
main'           :: [String] -> IO()
main' [] = do intMode
main' (x:y:xs) = do checkModes x y
main' xs = do putStrLn stratsPrint

-- Returns strategies
stratsPrint :: String
stratsPrint = "\nStrategies:\n  offensive\n  defensive\n  human\n"

stratsList :: [String]
stratsList = ["offensive", "defensive", "human"]

-- Interactive mode. Shows strategies and gets user input.
intMode :: IO()
intMode = do
  putStrLn ("\nEnter one of the strategies following strategies for each challenger:\n" ++ stratsPrint)
  putStrLn "Enter the black strategy: "
  blackStrat <- getLine
  putStrLn "Enter the white strategy: "
  whiteStrat <- getLine
  checkModes blackStrat whiteStrat

-- Checks if the input from either interactive mode or normal mode is correct and runs the game if it is.
checkModes :: [Char] -> [Char] -> IO()
checkModes x y
  | ((elem x stratsList) && (elem y stratsList))  = startGame x y
  | otherwise                                     = intMode

-- Gets input for the human turn and splits to input into ints. Used to do checkLen.
splitInts :: IO [Int]
splitInts = fmap (map read.words) getLine

-- Checks to make sure the user for the human strategy enters in four integer arguments to move. Will reprompt with an error message if they enter invalid input.
checkLen :: IO [Int]
checkLen = do
  ints <- splitInts
  if length ints == 4 then return ints else do
    putStrLn ("Too many or too few integers. Please enter four integers separated by spaces.")
    checkLen

-- Used in checkGreater to check if any of the values are greater than 4
greaterFour :: Ord a => Num a => a -> Bool
greaterFour x = (x <= 4)

-- Used in checkLesser to check if any of the values are less than zero
lesserZero :: Ord a => Num a => a -> Bool
lesserZero x = (x >= 0)

-- Checks if any values in the input list are greater than 4. Returns True if there is a value out of range.
checkGreater :: Ord a => Num a => [a] -> Bool
checkGreater [] = False
checkGreater (x:xs)
  | greaterFour x = checkGreater xs
  | otherwise     = True

-- Checks if any values in the input list are less than 4. Returns True if there is a value out of range.
checkLesser :: Ord a => Num a => [a] -> Bool
checkLesser [] = False
checkLesser (x:xs)
  | lesserZero x = checkLesser xs
  | otherwise    = True


-- Dummy for main loop. Replace with main loop when ready.
startGame :: String -> String -> IO()
startGame x y = putStrLn "\nmain"


---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)
