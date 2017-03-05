
module Util(
  PieceType,
  stratsPrint, stratsList, knightOffset, aggroPawnOffset, typeOf,
  fromIntegerPairList, getPieces, addPair, chooseRandomMove,
  checkIfSame, checkIfEqualPiece, checkValidM, isInBounds,
  checkKM, checkPM, checkBP, checkWP, isInBoundsForMoves, checkGameEnd,
  allPiecesOfType, checkPawnPos, checkPPOK, iterCor, getFirstCor, getFourthCor) where
  
import System.IO.Unsafe
import System.Random
import Data.Maybe (fromJust, isNothing)
import ApocTools
  
--PieceType Data type
data PieceType = Knight | Pawn deriving (Eq, Show, Read)                                     

--Utility Stuff
stratsPrint = "\nStrategies:\n  offensive\n  defensive\n  human\n"
stratsList = ["offensive", "defensive", "human"]

knightOffset = [(1,2), (1,-2), (-1,2), (-1,-2), (2,1), (-2,1), (2,-1), (-2,-1)]
aggroPawnOffset = [(1,1), (-1,1)]
  
--Returns the type of a piece.
typeOf :: Piece -> PieceType
typeOf BlackKnight = Knight
typeOf WhiteKnight = Knight
typeOf BlackPawn   = Pawn
typeOf WhitePawn   = Pawn

--Converting function to turn Integer to Int for a two item tuple
fromIntegerPairList :: [(Integer, Integer)] -> [(Int, Int)]
fromIntegerPairList [] = []
fromIntegerPairList ((x, y):ops) = ((fromIntegral x), (fromIntegral y)) : fromIntegerPairList ops

--Returns all pieces of a particular type
getPieces :: Board -> Cell -> Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
getPieces board c x y xmin ymin xmax ymax | (x == xmax && y == ymax) = if ((getFromBoard board) (xmax, ymax)) == c 
                                                                       then (x, y) : [] 
                                                                       else []
                                       | (x == xmax) = if ((getFromBoard board) (xmax, y)) == c
                                                          then (xmax, y) : getPieces board c xmin (y + 1) xmin ymin xmax ymax 
                                                          else getPieces board c xmin (y + 1) xmin ymin xmax ymax
                                       | otherwise = if ((getFromBoard board) (x, y)) == c
                                                        then (x, y) : getPieces board c (x + 1) y xmin ymin xmax ymax 
                                                        else getPieces board c (x + 1) y xmin ymin xmax ymax

--Adds a tuple Int pair
addPair :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPair one two = ((fst one + fst two), (snd one + snd two))

--Chooses and random move from a list of possible moves
chooseRandomMove :: [(Int, Int)] -> Maybe (Int, Int)
chooseRandomMove [] = Nothing
chooseRandomMove (op:ops) = Just (op)

--Checks if two positions have the same piece.
checkIfSame :: [(Int, Int)] -> [(Int, Int)] -> Bool
checkIfSame c1 c2 = (head c1 == head c2)

--Checks if two pieces on the board are equal or not.
checkIfEqualPiece :: Board -> [(Int, Int)] -> [(Int, Int)] -> Cell
checkIfEqualPiece b pos1 pos2 | (t1 == t2) = E
                              | (t1 == Knight) = p1
                              | otherwise = p2
                               where p1 = getFromBoard b (pos1 !! 0)
                                     p2 = getFromBoard b (pos2 !! 0)
                                     t1 = typeOf $ pieceOf p1
                                     t2 = typeOf $ pieceOf p2

--Checks if the movement given is valid.
checkValidM :: Maybe[(Int, Int)] -> Player -> GameState -> Bool
checkValidM Nothing p b = True
checkValidM m p b =  
    if isInBoundsForMoves m then
        if cell1==E || (p == White && (cell1==BK || cell1==BP)) || (p == Black && (cell1==WK || cell1==WP))
            then False
            else if (cell1==BK || cell1==WK) then checkKM m b else checkPM m p b
    else False
    where cell1 = (getFromBoard (theBoard b) ((fromJust m) !! 0))
    
-- Check if the m is within the bounds of the b
isInBounds :: (Int, Int) -> Bool
isInBounds m | a>4 = False
		| b>4 = False
                | a<0 = False
                | b<0 = False
                | otherwise = True
                where a = fst m
                      b = snd m

--Knight Check
checkKM :: Maybe[(Int,Int)] -> GameState -> Bool
checkKM km b | cell1 == E = False
                                   | (abs fv == 2 && abs sv == 1) || (abs fv == 1 && abs sv == 2) = True
                                   | dtp /= E && stp /= E && playerOf (pieceOf dtp) == playerOf(pieceOf(stp)) = False
                                   | otherwise = False
                                    where cell1 = (getFromBoard (theBoard b) ((fromJust km) !! 0))
                                          stp = (getFromBoard (theBoard b) ((fromJust km) !! 0))
                                          dtp = (getFromBoard (theBoard b) ((fromJust km) !! 1))
                                          fv = fst ((fromJust km) !! 0) - fst ((fromJust km) !! 1)
                                          sv = snd ((fromJust km) !! 0) - snd ((fromJust km) !! 1)
                                          
--Player Check
checkPM :: Maybe[(Int,Int)] -> Player -> GameState -> Bool
checkPM pm p b | p == Black = checkBP pm b
               | p == White = checkWP pm b

--Check Black player
checkBP :: Maybe[(Int,Int)] -> GameState -> Bool
checkBP pm b | (fv == 0 && sv == 1 && edge == E) = True
             | (fv == 1 && sv == 1 && edge /= E && playerOf (pieceOf edge) /= Black) = True 
             | (fv == -1 && sv == 1 && edge /= E && playerOf (pieceOf edge) /= Black) = True
             | otherwise = False
             where edge = (getFromBoard (theBoard b) ((fromJust pm) !! 1))
                   fv = fst ((fromJust pm) !! 0) - fst ((fromJust pm) !! 1)
                   sv = snd ((fromJust pm) !! 0) - snd ((fromJust pm) !! 1)

--Check White player
checkWP :: Maybe[(Int,Int)] -> GameState -> Bool
checkWP pm b | (fv == 0 && sv == -1 && edge == E) = True
             | (fv == 1 && sv == -1 && edge /= E && playerOf (pieceOf edge) /= White) = True 
             | (fv == -1 && sv == -1 && edge /= E && playerOf (pieceOf edge) /= White) = True
             | otherwise = False
              where edge = (getFromBoard (theBoard b) ((fromJust pm) !! 1))
                    fv = fst ((fromJust pm) !! 0) - fst ((fromJust pm) !! 1)
                    sv = snd ((fromJust pm) !! 0) - snd ((fromJust pm) !! 1)

--Checks if move is in bound
isInBoundsForMoves      :: Maybe[(Int, Int)] -> Bool
isInBoundsForMoves m 
                 | ff>4 = False
                 | ff<0 = False
                 | fs>4 = False
                 | fs<0 = False
                 | sf>4 = False
                 | sf<0 = False
                 | ss>4 = False
                 | ss<0 = False
                 | otherwise = True
                 where ff = fst ((fromJust m) !! 0)
                       fs = snd ((fromJust m) !! 0)
                       sf = fst ((fromJust m) !! 1)
                       ss = snd ((fromJust m) !! 1) 

--Check for end game conditions
checkGameEnd :: GameState -> Bool
checkGameEnd b | (whitePen b) >= 2 ||(blackPen b) >= 2 = True 
checkGameEnd b =  if (elem '+' (board2Str (theBoard b))) && (elem '/' (board2Str (theBoard b)))
                    then False
                    else True   

--Returns all the positions of a piece
allPiecesOfType :: Board -> Player -> String -> [(Int, Int)]
allPiecesOfType b p pt | (p == Black && pt == "Knight") = getPieces b BK 0 0 0 0 4 4
                       | (p == Black && pt == "Pawn")   = getPieces b BP 0 0 0 0 4 4 
                       | (p == White && pt == "Knight") = getPieces b WK 0 0 0 0 4 4
                       | (p == White && pt == "Pawn")   = getPieces b WP 0 0 0 0 4 4 

--Check the position of a pawn
checkPawnPos :: [Cell] -> Piece -> Int -> Int -> (Int,Int)
checkPawnPos [] pc xCor yCor = (-1,-1)
checkPawnPos (x:xs) pc xCor yCor = if (x /= E && (pieceOf x) == pc) 
    then (xCor,yCor)
    else checkPawnPos xs pc (xCor+1) yCor 

--Check Pawn movement okay
checkPPOK :: Maybe [(Int, Int)] -> GameState -> Bool
checkPPOK m b | m == Nothing = True
              | getFromBoard (theBoard b) ((fromJust m) !! 0) == E = True
              | otherwise = False

--Iterate through a row and check if piece is there
iterCor :: [Cell] -> Piece -> IO Bool
iterCor [] pc = return False
iterCor (x:xs) pc = if (x /= E && (pieceOf x) == pc) 
    then return True
    else iterCor xs pc

--Get the first row
getFirstCor :: Board -> [Cell]
getFirstCor (x:xs) = x

--Get the last row
getFourthCor :: Board -> [Cell]
getFourthCor (x:xs:xss:xsss:xssss:[]) = xssss



















