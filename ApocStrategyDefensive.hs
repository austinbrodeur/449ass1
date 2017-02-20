{-|
Module: ApocStrategyDefensive
Description: A defensive AI strategy for Apocalypse

This strategies' goal is to make the safest possible plays. It does so by
only making moves which cannot be reached by an opponent in the next turn.
This strategy is quite heavily based on the offensive strategy with some added
checks and different weight on safer moves.

-}

module ApocStrategyDefensive where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.IO.Unsafe
import System.Environment
import ApocTools
import Checks

defensive :: Chooser
-- * Game state based on a normal play, which would be moving a knight.  
defensive board Normal White =
    let move = chooseRandomMove ((validMoves board knightOffset (getPieces board White WK 0 0 0 0 4 4)) ++ (validMoves board White pawnOffset (getPieces board WP 0 0 0 0 4 4))) in
    
    if (move == Nothing)
       then return Nothing
       else return $ Just [(fromJust move)]
      
defensive board Normal Black =
    let move = chooseRandomMove ((validMoves board Black knightOffset (getPieces board BK 0 0 0 0 4 4)) ++ (validMoves board Black pawnOffset (getPieces board BP 0 0 0 0 4 4))) in
    
    if (move == Nothing)
       then return Nothing
       else return $ Just [(fromJust move)]      
    
-- * Game state based on a pawn move
defensive board PawnPlacement player =
     let move = chooseRandomMove (getPieces board E 0 0 0 1 4 3)
     
     if (move == Nothing)
        then return Nothing
        else return $ Just [(fromJust move)]

validMoves :: GameState -> Player -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
validMoves board player [] _ = []
validMoves board player _ [] = []
validMoves board player offsets (op:ops) = (goodMoves board player offsets op) ++ (validMoves board player offsets ops)

goodMoves :: GameState -> Player -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
goodMoves board player (off:offs) op | (check == WP) && (player == White) = goodMoves board player offs op
                                     | (check == WK) && (player == White) = goodMoves board player offs op
                                     | (check == BP) && (player == Black) = goodMoves board player offs op
                                     | (check == BK) && (player == Black) = goodMoves board player offs op
                                     | otherwise = move : goodMoves board player offs op
                                     where move = (addPair op off)
                                           check = (getFromBoard (theBoard board) move)

getPieces :: GameState -> Cell -> Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
getPieces board c x y xmin ymin xmax ymax | (x == xmax && y == ymax) = if ((getFromBoard (theBoard board)) (xmax, ymax)) == c 
                                                                       then (x, y) : [] 
                                                                       else []
                                       | (x == xmax) = if ((getFromBoard (theBoard board)) (xmax, y)) == p 
                                                          then (xmax, y) : getPieces board c xmin (y + 1) xmin ymin xmax ymax 
                                                          else getPieces board c xmin (y + 1) xmin ymin xmax ymax
                                       | otherwise = if ((getFromBoard (theBoard board)) (x, y)) == p 
                                                        then (x, y) : getPieces board c (x + 1) y xmin ymin xmax ymax 
                                                        else getPieces board c (x + 1) y xmin ymin xmax ymax

addPair :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPair one two = ((fst one + fst two), (snd one + snd two))

chooseRandomMove :: [(Int, Int)] -> Maybe (Int, Int)
chooseRandomMove [] = Nothing
chooseRandomMove ops = let i = unsafePerformIO(getStdRandom(randomR(0, length ops))) in
                               Just(ops !! i)
