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
defensive gameState Normal player =
    let board = theBoard gameState in
    let defensiveMoves = (orderDefensiveMoves board player (piecesAtRisk board player)) in
    if (length defensiveMoves > 0) && (randomChance 0.7) then
      let move = chooseRandomMove defensiveMoves 0.75 in
      if move /= Nothing then
          return $ Just [(fst (fromJust move)), (snd (fromJust move))]
      else return Nothing
    else
      let captureKnightPlay = (orderCapturePlay board player (allPiecesOfType board player "Knight"))
          capturePawnPlay = (orderCapturePlay board player (allPiecesOfType board player "Pawn"))
          noCapturePlay = (orderCapturePlay board player (allPieces board player))
          move = chooseRandomMove (captureKnightPlay ++ capturePawnPlay ++ noCapturePlay) 0.75 in
      if move /= Nothing then
          return $ Just [(fst (fromJust move)), (snd (fromJust move))]
      else return Nothing
      
-- * Game state based on a pawn move
defensive gameState PawnPlacement player =
  let emptyPieces = safeSquares (theBoard gameState) player (pieces (theBoard gameState) E 0 0 0 1 4 3) in
  let move = chooseMove emptyPieces in
  if move /= Nothing then
    return $ Just [(fromJust move)]
  else return Nothing

{- Move options
-}


{- Plays that do not end in a capture
-}

-- |Returns a list of the best no capture plays available on the board
noCaptureMove :: Board -> Player -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
noCaptureMove b player [] = []
noCaptureMove b player (p:ps) | (best == Nothing) = (noCaptureMove b player ps)
                                  | otherwise = (p, fromJust best) : (noCaptureMove b player ps)
                                  where movesList = allNotKills b p
                                        best = chooseRandomMove (orderNoCapture b player movesList) 0.75

-- Returns a list of no capture plays
orderNoCapture :: Board -> Player -> [(Int, Int)] -> [(Int, Int)]
orderNoCapture b player [] = []
orderNoCapture b player (p:ps) | isSafePosition b player p = p : (orderNoCapture b player ps)
                              | otherwise = (orderNoCapture b player ps) ++ [p]


-- | Returns a list of moves that will take a piece at risk and make it safe
orderDefensiveMoves :: Board -> Player -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
orderDefensiveMoves b player [] = []
orderDefensiveMoves b player (p:ps) | (bestMove == Nothing) = orderDefensiveMoves b player ps
                                  | otherwise = (p, fromJust bestMove) : orderDefensiveMoves b player ps
                                  where movesList = getMoves b p True
                                        bestMove = chooseRandomMove(orderDefense b player movesList) 0.75


-- | Returns a list of safe places we are able to reach on the board
orderDefense :: Board -> Player -> [(Int, Int)] -> [(Int, Int)]
orderDefense b player [] = []
orderDefense b player (m:ms) | not (safeTile b player m) = m : orderDefense b player ms
                           | otherwise = orderDefense b player ms


{- Plays that end in a capture
-}

-- |Returns a list of the best moves possible which will capture an eneemy piece
-- 
orderCapturePlay :: Board -> Player -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
orderCapturePlay b player [] = []
orderCapturePlay b player (p:ps) | (bestKillPiece == Nothing) = (orderCapturePlay b player ps)
                        | (isSafePosition b player (fromJust bestKill)) = [(p, fromJust bestKill)] ++ orderCapturePlay b player ps
                        | (fromJust bestKillPiece == Knight) = orderCapturePlay b player ps ++ [(p, fromJust bestKill)]
                        | (fromJust bestKillPiece == Pawn)   = orderCapturePlay b player ps ++ [(p, fromJust bestKill)]
                        where killsList = allKills b p
                              bestKill = chooseRandomMove (orderCapture b player killsList) 0.75
                              bestKillPiece | (bestKill == Nothing) = Nothing
                                            | otherwise = Just $ typeOf (pieceOf (getFromBoard b (fromJust bestKill)))


-- |Return a list of pieces able to be capture in order of importance (Knight, Pawn)
orderCapture :: Board -> Player -> [(Int, Int)] -> [(Int, Int)]
orderCapture board player [] = []
orderCapture board player (k:ks) | safeTile board player k = k : (orderCapture board player ks)
                        | (piece == Knight) = (orderCapture board player ks) ++ [k]
                        | (piece == Pawn)   = (orderCapture board player ks) ++ [k]
                        where piece = typeOf (pieceOf (getFromBoard board k))


{- Pieces in danger and safe spots
-}

-- |Returns a list of the current safe spaces on the board 
safeSquares :: Board -> Player -> [(Int, Int)] -> [(Int, Int)]
safeSquares b player [] = []
safeSquares b player (x:xs) | (safeTile b player x) = x : safeSquares b player xs
                              | otherwise = safeSquares b player xs


-- |Returns a list of pieces in danger on the board
piecesAtRisk :: Board -> Player -> [(Int, Int)]
piecesAtRisk b player = allPiecesAtRisk b player ((allPiecesOfType b player "Pawn") ++ (allPiecesOfType b player "Knight"))


-- |This method returns a list of pieces in danger of being captured
allPiecesAtRisk :: Board -> Player -> [(Int, Int)] -> [(Int, Int)]
allPiecesAtRisk b player [] = []
allPiecesAtRisk b player (x:xs) | not (safeTile b player x) = x : allPiecesAtRisk b player xs
                                  | otherwise = allPiecesAtRisk b player xs
