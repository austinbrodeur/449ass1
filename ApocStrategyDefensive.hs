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
import System.IO.Unsafe
import System.Environment
import Data.Char
import ApocTools
import MoreApocTools
import System.Random

safe :: Chooser

safe gameState Normal player =
    let board = theBoard gameState in

    let defensiveMoves = (orderDefensiveMoves board player (piecesAtRisk board player)) in

    if (length defensiveMoves > 0) && (randomChance 0.7) then
      let move = chooseFromWeightedList defensiveMoves 0.75 in
      if move /= Nothing then
          return $ Just [(fst (fromJust move)), (snd (fromJust move))]
      else return Nothing
    else
      let captureKnightPlay = (orderCapturePlay board player (allPiecesOfType board player Knight))
          capturePawnPlay = (orderCapturePlay board player (allPiecesOfType board player Pawn))
          noCapturePlay = (orderCapturePlay board player (allPieces board player))
          move = chooseFromWeightedList (captureKnightPlay ++ capturePawnPlay ++ noCapturePlay) 0.75 in
      if move /= Nothing then
          return $ Just [(fst (fromJust move)), (snd (fromJust move))]
      else return Nothing

safe gameState PawnPlacement player =
  let emptyPieces = safeSquares (theBoard gameState) player (pieces (theBoard gameState) E 0 0 0 1 4 3) in

  let move = chooseFromList emptyPieces in

  if move /= Nothing then
    return $ Just [(fromJust move)]
  else return Nothing

{- Move options
-}

-- |Returns a list of all the positions from the given list that are a safe place for a player to have a piece
safeSquares :: Board -> Player -> [(Int, Int)] -> [(Int, Int)]
safeSquares b player [] = []
safeSquares b player (x:xs) | (isSafePosition b player x) = x : safeSquares b player xs
                              | otherwise = safeSquares b player xs

-- |Returns a list of all the pieces of a player on the board that are in danger
piecesAtRisk :: Board -> Player -> [(Int, Int)]
piecesAtRisk b player = allPiecesAtRisk b player ((allPiecesOfType b player Pawn) ++ (allPiecesOfType b player Knight))

-- |Returns a list from a given list of pieces that are in danger
allPiecesAtRisk :: Board -> Player -> [(Int, Int)] -> [(Int, Int)]
allPiecesAtRisk b player [] = []
allPiecesAtRisk b player (x:xs) | not (isSafePosition b player x) = x : allPiecesAtRisk b player xs
                                  | otherwise = allPiecesAtRisk b player xs

-- |Returns a list of moves that will take a piece in danger and move it to safe place
orderDefensiveMoves :: Board -> Player -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
orderDefensiveMoves b player [] = []
orderDefensiveMoves b player (p:ps) | (bestMove == Nothing) = orderDefensiveMoves b player ps
                                  | otherwise = (p, fromJust bestMove) : orderDefensiveMoves b player ps
                                  where movesList = getMoves b p True
                                        bestMove = chooseFromWeightedList (orderDefense b player movesList) 0.75

-- |Returns a list based on a given list of positions that are all safe positions for a player
orderDefense :: Board -> Player -> [(Int, Int)] -> [(Int, Int)]
orderDefense b player [] = []
orderDefense b player (m:ms) | not (isSafePosition b player m) = m : orderDefense b player ms
                           | otherwise = orderDefense b player ms


{- Plays that end in a capture
-}

-- |Returns an ordered list of the "best" moves that will result in a kill that can be made
-- by a piece given by the supplied coordinate
orderCapturePlay :: Board -> Player -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
orderCapturePlay b player [] = []
orderCapturePlay b player (p:ps) | (bestKillPiece == Nothing) = (orderCapturePlay b player ps)
                        | (isSafePosition b player (fromJust bestKill)) = [(p, fromJust bestKill)] ++ orderCapturePlay b player ps
                        | (fromJust bestKillPiece == Knight) = orderCapturePlay b player ps ++ [(p, fromJust bestKill)]
                        | (fromJust bestKillPiece == Pawn)   = orderCapturePlay b player ps ++ [(p, fromJust bestKill)]
                        where killsList = allKills b p
                              bestKill = chooseFromWeightedList (orderCapture b player killsList) 0.75
                              bestKillPiece | (bestKill == Nothing) = Nothing
                                            | otherwise = Just $ typeOf (pieceOf (getFromBoard b (fromJust bestKill)))

-- |Orders a list of "kill" moves based on whether they kill a Knight or a Pawn
orderCapture :: Board -> Player -> [(Int, Int)] -> [(Int, Int)]
orderCapture board player [] = []
orderCapture board player (k:ks) | isSafePosition board player k = k : (orderCapture board player ks)
                        | (piece == Knight) = (orderCapture board player ks) ++ [k]
                        | (piece == Pawn)   = (orderCapture board player ks) ++ [k]
                        where piece = typeOf (pieceOf (getFromBoard board k))

{- Plays that do not end in a capture
-}

-- |Returns an ordered list of the "best" moves that will not result in a kill that can be made
-- by a piece given by the supplied coordinate
noCaptureMove :: Board -> Player -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
noCaptureMove b player [] = []
noCaptureMove b player (p:ps) | (best == Nothing) = (noCaptureMove b player ps)
                                  | otherwise = (p, fromJust best) : (noCaptureMove b player ps)
                                  where movesList = allNotKills b p
                                        best = chooseFromWeightedList (orderNoCapture b player movesList) 0.75

-- |Orders a list of not kill moves. This is a placeholder which does nothing because the greedy
-- method is all about the kills, so it doesn't order this list at all
orderNoCapture :: Board -> Player -> [(Int, Int)] -> [(Int, Int)]
orderNoCapture b player [] = []
orderNoCapture b player (p:ps) | isSafePosition b player p = p : (orderNoCapture b player ps)
                              | otherwise = (orderNoCapture b player ps) ++ [p]