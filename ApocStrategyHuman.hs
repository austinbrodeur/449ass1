{- |
Module      : ApocStrategyHuman
Description : Template for a game-playing strategy definition.
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
This is merely a skeleton to get you started on creating a strategy for playing the
Apocalypse game.  It has VERY little functionality.
-}

module ApocStrategyHuman where

import System.IO.Unsafe
import Data.Maybe (fromJust, isNothing)
import Data.Char
import ApocTools

{- human strategy takes input from user and parses to return an IO(Maybe[(Int, Int)]). -}
human    :: Chooser
human board Normal player = do
    if player == Black 
    then putStrLn $ "Enter the movement coordinates for player Black or just enter to pass B2:"
    else putStrLn $ "Enter the movement coordinates for player White or just enter to pass W2:"
    
    input <- getLine
    let inplist = convertToInt input
    if length inplist == 0 
       then return Nothing 
       else return (Just [((inplist !! 0), (inplist !! 1)), ((inplist !! 2), (inplist !! 3))])
       
human board PawnPlacement player = do
    if player == Black 
    then putStrLn $ "Enter the pawn movement coordinates for player Black or just enter to pass B1:"
    else putStrLn $ "Enter the pawn movement coordinates for player White or just enter to pass W1:"
    
    input <- getLine
    let inplist = convertToInt input
    if length inplist == 0 
       then return Nothing 
       else return (Just [((inplist !! 0), (inplist !! 1))])

{- function to translate the input into an integer array -}
convertToInt :: String -> [Int]
convertToInt = map read . words
