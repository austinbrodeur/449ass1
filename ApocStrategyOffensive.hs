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
import MoreApocTools

-- | The offensive chooser function. It takes in the board state
-- and takes the most aggressive play
offensive :: Chooser

offensive gameState Normal player
     let board = theBoard gameState in 
     let captureKnight = (killPlay board (allPiecesOfType board player Knight)) in
     let capturePawn = (killPlay board (allPiecesOfType board player Pawn)) in
     let noCapture = (noKillPlay board (allPieces board player)) in
     let actualMove = chooseRandomMove (captureKnight ++ capturePawn ++ noCapture) 0.5 in
     if actualMove /= Nothing then
       return $ Just [(fromJust actualMove)]
     else return Nothing
     
     
offensive gameState PawnPlacement player =
  let emptyPlaces = pieces (theBoard gameState) E 0 0 0 1 4 3 in
  let move = chooseMove emptyPlaces in
  if move /= Nothing then
    return $ Just [(fromJust move)]
  else return Nothing
     
     
{---------Movement options---------}
     
-- | Takes in the board state and initial coordinates and returns a list of 
-- the optimal moves that will capture a piece
killPlay :: Board -> [(Int, Int)] -> [((Int, Int), (Int, Int,))]
killPlay b [] = []
killPlay b (p:ps) | (idealKillPiece == Nothing) = (killPlay b ps)
                  | (fromJust idealKillPiece Knight) = [(p, fromJust idealKill)] ++ killPlay b ps
                  | (fromJust idealKillPiece Pawn) = killPlay b ps + [(p, fromJust idealKill)]
                  where possibleKills = totalKills b p
                        idealKill = chooseRandomMove (capture b possibleKills) 0.5
                        idealKillPiece | (idealKill == Nothing) = Nothing
                                       | otherwise = Just $ typeOf (pieceOf (getFromBoard b (fromJust idealKill)))
                           
                           
                           
-- | Puts a list of captures available in order of importance (Knight over Pawn)
capture :: Board -> [(Int, Int)] -> [(Int, Int)]
capture board [] = []
capture board (k:ks) | (piece == Knight) = [k] ++ (capture board ks)
                     | (piece == Pawn) = (capture board ks) ++ [k]
                     where piece = typeOf (pieceOf (getFromBoard board k))
                     
         
-- | Takes in the board state and initial coordinates and returns a list of 
-- the optimal moves that will not capture a piece       
noKillPlay :: Board -> [(Int, Int)] -> [((Int, Int), (Int, Int,))]
noKillPlay b [] = []
noKillPlay b (p:ps) | (best == Nothing) = (noKillPlay b ps)
                    | otherwise = (p, fromJust best) : (noKillPlay b ps)
                    where movesList = noKill b p
                          best = chooseRandomMove (noKill b moves) 0.5
                          
--| Simply a placeholder method because this strategy only tries to capture pieces
-- so there is no need to order the non-capture moves, simply pick one and continue
noKill :: Board -> [(Int, Int)] -> [(Int, Int)]
noKill b ps = ps
