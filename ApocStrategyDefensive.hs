{-|
Module: ApocStrategyDefensive
Description: A defensive AI strategy for Apocalypse
This strategies' goal is to make the safest possible plays. It does so by
only making moves which cannot be reached by an opponent in the next turn.
This strategy is quite heavily based on the offensive strategy with some added
checks and different weight on safer moves.
-}

module ApocStrategyDefensive where

import System.Environment
import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.Random
import System.IO.Unsafe
import ApocTools
import Util

defensive :: Chooser
-- * Game state based on a normal play, which would be moving a knight.  
defensive board Normal White =
    let move = chooseRandomMove ((validMoves board White (fromIntegerPairList knightOffset) (getPieces (theBoard board) WK 0 0 0 0 4 4)) ++ (validMoves board White (fromIntegerPairList ((1,1) : aggroPawnOffset)) (getPieces (theBoard board) WP 0 0 0 0 4 4))) in
    
    if (move == Nothing)
       then return Nothing
       else return $ Just [(fromJust move)]
      
defensive board Normal Black =
    let move = chooseRandomMove ((validMoves board Black (fromIntegerPairList knightOffset) (getPieces (theBoard board) BK 0 0 0 0 4 4)) ++ (validMoves board Black (fromIntegerPairList ((1,1) : aggroPawnOffset)) (getPieces (theBoard board) BP 0 0 0 0 4 4))) in
    
    if (move == Nothing)
       then return Nothing
       else return $ Just [(fromJust move)]      
    
-- * Game state based on a pawn move
defensive board PawnPlacement player =
     let move = chooseRandomMove (getPieces (theBoard board) E 0 0 0 1 4 3) in
     if (move == Nothing)
        then return Nothing
        else return $ Just [(fromJust move)]

validMoves :: GameState -> Player -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
validMoves board player [] _ = []
validMoves board player _ [] = []
validMoves board player offsets (op:ops) = (goodMoves board player offsets op) ++ (validMoves board player offsets ops)

goodMoves :: GameState -> Player -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
goodMoves board player [] op = []
goodMoves board player (off:offs) op | (check == WP) && (player == White) = goodMoves board player offs op
                                     | (check == WK) && (player == White) = goodMoves board player offs op
                                     | (check == BP) && (player == Black) = goodMoves board player offs op
                                     | (check == BK) && (player == Black) = goodMoves board player offs op
                                     | otherwise = move : goodMoves board player offs op
                                     where move = (addPair op off)
                                           check = (getFromBoard (theBoard board) move)
