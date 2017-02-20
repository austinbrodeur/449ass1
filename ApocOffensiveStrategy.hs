{- 
Module: ApocStrategyOffensive
Description: Aggressively styled AI

The aggressive strategy is always looking to capture enemy pieces, regardless of whether
the play is intelligent or not. It searches the game board to find any possible capture 
and takes it. If it finds no plays which will capture a piece, it simply makes the first non-
capture move calculated, as the noKill method is essentially a placeholder.
-}

module ApocStrategyOffensive where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.Environment
import ApocTools
import Checks

{- offensive strategy always tries to go for kill moves. If none are available it moves randomly.. returns an IO(Maybe[(Int, Int)]). -}
offensive :: Chooser

offensive board Normal White = 
     let validMoves = (checkKillMoves board (getPieces board WK 0 0 0 4 4)) in
     if (validMoves /= [])
        then return Just [(fromJust (head validMoves))]
        else let validMoves = (checkKillMoves board (getPieces board WP 0 0 0 4 4)) in
             if (validMoves /= [])
                then return Just [(fromJust (head validMoves))]
                else let validMoves = (getValidMoves board White) in
                     if (validMoves /= [])
                        then return Just [(fromJust (head validMoves))]
                        else return Nothing

offensive board Normal Black =
     let validMoves = (checkKillMoves board (getPieces board BK 0 0 0 4 4)) in
     if (validMoves /= [])
        then return Just [(fromJust (head validMoves))]
        else let validMoves = (checkKillMoves board (getPieces board BP 0 0 0 4 4)) in
             if (validMoves /= [])
                then return Just [(fromJust (head validMoves))]
                else let validMoves = (getValidMoves board Black) in
                     if (validMoves /= [])
                        then return Just [(fromJust (head validMoves))]
                        else return Nothing

     
offensive board PawnPlacement player =
     let move = chooseRandomMove (getPieces board E 0 0 0 1 4 3)
     
     if (move == Nothing)
        then return Nothing
        else return Just [(fromJust move)]

checkKillMoves :: GameState -> [(Int, Int)] -> [(Int, Int)]
checkKillMoves board [] = []
checkKillMoves board (op:ops) = if (getFromBoard (theBoard board) op == WK)
                                   then checkKill board knightOffset op : checkKillMoves board ops
                                   else checkKill board pawnOffset op : checkKillMoves board ops
                     
checkKill :: GameState -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
checkKill board [] _ = []
checkKill board (off:offs) op = if ((getFromBoard (theBoard board) (addPair op off)) == E)

addPair :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPair one two = ((fst one + fst two), (snd one + snd two))

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
     
chooseRandomMove :: [(Int, Int)] -> Maybe (Int, Int)
chooseRandomMove [] = Nothing
chooseRandomMove ops = let i = unsafePerformIO(getStdRandom(randomR(0, length ops))) in
                               Just(ops !! i)
