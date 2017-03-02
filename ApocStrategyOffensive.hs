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
import System.Random
import System.IO.Unsafe

{- offensive strategy always tries to go for kill moves. If none are available it moves randomly.. returns an IO(Maybe[(Int, Int)]). -}
offensive :: Chooser

offensive board Normal White = 
     let validMoves = (checkKillMoves board White (getPieces board WK 0 0 0 0 4 4)) in
     if (validMoves /= [])
        then return $ Just [((head validMoves))]
        else let validMoves = (checkKillMoves board White (getPieces board WP 0 0 0 0 4 4)) in
             if (validMoves /= [])
                then return $ Just [((head validMoves))]
                else let validMoves = (getEmptyMoves board White) in
                     if (validMoves /= [])
                        then return $ Just [(fromJust (chooseRandomMove validMoves))]
                        else return Nothing

offensive board Normal Black =
     let validMoves = (checkKillMoves board Black (getPieces board BK 0 0 0 0 4 4)) in
     if (validMoves /= [])
        then return $ Just [((head validMoves))]
        else let validMoves = (checkKillMoves board Black (getPieces board BP 0 0 0 0 4 4)) in
             if (validMoves /= [])
                then return $ Just [((head validMoves))]
                else let validMoves = (getEmptyMoves board Black) in
                     if (validMoves /= [])
                        then return $ Just [(fromJust (chooseRandomMove validMoves))]
                        else return Nothing

     
offensive board PawnPlacement player =
     let move = chooseRandomMove (getPieces board E 0 0 0 1 4 3) in
     if (move == Nothing)
        then return Nothing
        else return $ Just [(fromJust move)]

checkKillMoves :: GameState -> Player -> [(Int, Int)] -> [(Int, Int)]
checkKillMoves board player [] = []
checkKillMoves board player (op:ops) = if ((getFromBoard (theBoard board) op == WK) || (getFromBoard (theBoard board) op == BK))
                                          then (checkKill board player knightOffset op) ++ (checkKillMoves board player ops)
                                          else (checkKill board player (fromIntegerPairList (aggroPawnOffset)) op) ++ (checkKillMoves board player ops)
                     
checkKill :: GameState -> Player -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
checkKill board player [] _ = []
checkKill board player (off:offs) op | (check == WK) && (player == Black) = move : checkKill board player offs op
                                     | (check == BK) && (player == White) = move : checkKill board player offs op
                                     | (check == WP) && (player == Black) = checkKill board player offs op ++ [move]
                                     | (check == BP) && (player == White) = checkKill board player offs op ++ [move]
                                     | otherwise = checkKill board player offs op
                                     where move = (addPair op off)
                                           check = (getFromBoard (theBoard board) move)

getEmptyMoves :: GameState -> Player -> [(Int, Int)]
getEmptyMoves board White = (goodMoves board knightOffset (getPieces board WK 0 0 0 0 4 4)) ++ (goodMoves board [(1, 1)] (getPieces board WP 0 0 0 0 4 4))
getEmptyMoves board Black = (goodMoves board knightOffset (getPieces board BK 0 0 0 0 4 4)) ++ (goodMoves board [(1, 1)] (getPieces board BP 0 0 0 0 4 4))

goodMoves :: GameState -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
goodMoves board offsets [] = []
goodMoves board offsets (op:ops) = (possMove board offsets op) ++ (goodMoves board offsets ops)

possMove :: GameState -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
possMove board [] op = []
possMove board (off:offs) op | (check == E) = move : possMove board offs op
                             | otherwise = possMove board offs op
                             where move = (addPair op off)
                                   check = (getFromBoard (theBoard board) move)

addPair :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPair one two = ((fst one + fst two), (snd one + snd two))

getPieces :: GameState -> Cell -> Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
getPieces board c x y xmin ymin xmax ymax | (x == xmax && y == ymax) = if ((getFromBoard (theBoard board)) (xmax, ymax)) == c 
                                                                       then (x, y) : [] 
                                                                       else []
                                       | (x == xmax) = if ((getFromBoard (theBoard board)) (xmax, y)) == c 
                                                          then (xmax, y) : getPieces board c xmin (y + 1) xmin ymin xmax ymax 
                                                          else getPieces board c xmin (y + 1) xmin ymin xmax ymax
                                       | otherwise = if ((getFromBoard (theBoard board)) (x, y)) == c 
                                                        then (x, y) : getPieces board c (x + 1) y xmin ymin xmax ymax 
                                                        else getPieces board c (x + 1) y xmin ymin xmax ymax
     
chooseRandomMove :: [(Int, Int)] -> Maybe (Int, Int)
chooseRandomMove [] = Nothing
chooseRandomMove (op:ops) = Just (op)
