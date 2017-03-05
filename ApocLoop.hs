
module ApocLoop(
    --Game Stuff
    gameLoop, doPP, finallyDone, checkPawnAtTheEdge, pawn2KnightB, pawn2KnightW
    ) where

import Data.List
import Data.Maybe (fromJust, isNothing)
import Data.Char
import ApocTools
import Util
import ApocStrategyHuman
import ApocStrategyDefensive
import ApocStrategyOffensive

--The main loop for a single round.
gameLoop :: GameState -> Chooser -> String -> Chooser -> String -> IO()
gameLoop b bA bAS wA wAS = do
    blackMovement <- bA (b) Normal Black 
    whiteMovement <- wA (b) Normal White
    let boardChanged = GameState (if blackMovement==Nothing
                                then Passed
                                else if checkValidM blackMovement Black b 
                                  then Played (head (fromJust blackMovement), head (tail (fromJust blackMovement)))
                                  else Goofed (head (fromJust blackMovement), head (tail (fromJust blackMovement))))
                               (blackPen b + if ((checkValidM blackMovement Black b) == False) then 1 else 0)
                               (if whiteMovement==Nothing
                                  then Passed
                                  else if checkValidM whiteMovement White b 
                                    then Played (head (fromJust whiteMovement), head (tail (fromJust whiteMovement)))
                                    else Goofed (head (fromJust whiteMovement), head (tail (fromJust whiteMovement))))
                               (whitePen b + if ((checkValidM whiteMovement White b) == False) then 1 else 0)
                               (if (blackMovement==Nothing || checkValidM blackMovement Black b == False)
                                  then (if whiteMovement==Nothing || checkValidM whiteMovement White b == False then (theBoard b) else (replace2 (replace2 (theBoard b)
                                                   ((fromJust whiteMovement) !! 1)
                                                   (getFromBoard (theBoard b) ((fromJust whiteMovement) !! 0)))
                                                   ((fromJust whiteMovement) !! 0)
                                                   E))
                                  else (if whiteMovement==Nothing || checkValidM whiteMovement White b == False then (replace2 (replace2 (theBoard b)
                                                   ((fromJust blackMovement) !! 1)
                                                   (getFromBoard (theBoard b) ((fromJust blackMovement) !! 0)))
                                         ((fromJust blackMovement) !! 0) E)
                                          else if checkIfSame (fromJust blackMovement) (fromJust whiteMovement)
                                            then (replace2
                                                    (replace2
                                                        (replace2
                                                            (theBoard b) ((fromJust whiteMovement) !! 0) E)
                                                    ((fromJust blackMovement) !! 0) E)
                                              ((fromJust blackMovement) !! 1) (checkIfEqualPiece (theBoard b) (fromJust blackMovement) (fromJust whiteMovement)))
                                            else (replace2
                                                    (replace2
                                                      (replace2
                                                        (replace2
                                                          (theBoard b) ((fromJust whiteMovement) !! 0) E)
                                                        ((fromJust blackMovement) !! 0) E)
                                                      ((fromJust whiteMovement) !! 1) (getFromBoard (theBoard b) ((fromJust whiteMovement) !! 0)))
                                                    ((fromJust blackMovement) !! 1) (getFromBoard (theBoard b) ((fromJust blackMovement) !! 0)))
                                            ))
                                            
    putStrLn (show boardChanged)   
    if (whiteMovement == Nothing) && (blackMovement == Nothing) || checkGameEnd boardChanged
    then putStrLn(finallyDone boardChanged)
    else checkPawnAtTheEdge boardChanged bA bAS wA wAS        

--Does the pawn placement
doPP:: GameState -> Chooser -> String -> Chooser -> String -> Bool -> Bool -> IO()
doPP b bA bAS wA wAS False False = gameLoop b bA bAS wA wAS
doPP b bA bAS wA wAS boolForBlack boolForWhite = do
    blackMovement <- if boolForBlack && length (allPiecesOfType (theBoard b) Black "Knight") == 2
      then bA b PawnPlacement Black 
      else return Nothing
    whiteMovement <- if boolForWhite && length (allPiecesOfType (theBoard b) White "Knight") == 2
      then wA b PawnPlacement White 
      else return Nothing
    let changedB = GameState (if boolForBlack == False
                                 then None
                                 else if blackMovement == Nothing
                                    then if length (allPiecesOfType (theBoard b) Black "Knight") <2
                                        then UpgradedPawn2Knight (checkPawnPos (getFirstCor (theBoard b)) BlackPawn 0 0)
                                        else NullPlacedPawn
                                    else if checkPPOK blackMovement b
                                        then PlacedPawn ((checkPawnPos (getFirstCor (theBoard b)) BlackPawn 0 0), ((fromJust blackMovement) !! 0))
                                        else BadPlacedPawn ((checkPawnPos (getFirstCor (theBoard b)) BlackPawn 0 0), ((fromJust blackMovement) !! 0)))
                              (blackPen b + if (boolForBlack && ((blackMovement == Nothing && length (allPiecesOfType (theBoard b) Black "Knight") == 2)  || (checkPPOK blackMovement b)==False)) then 1 else 0)
                              (if boolForWhite==False
                                 then None
                                 else if whiteMovement==Nothing
                                    then if length (allPiecesOfType (theBoard b) White "Knight") < 2 
                                        then UpgradedPawn2Knight (checkPawnPos (getFourthCor (theBoard b)) WhitePawn 0 4)
                                        else NullPlacedPawn 
                                    else if checkPPOK whiteMovement b
                                        then PlacedPawn ((checkPawnPos (getFourthCor (theBoard b)) WhitePawn 0 4), ((fromJust whiteMovement) !! 0))
                                        else BadPlacedPawn ((checkPawnPos (getFourthCor (theBoard b)) WhitePawn 0 4), ((fromJust whiteMovement) !! 0)))
                              (whitePen b + if (boolForWhite && ((whiteMovement == Nothing && length (allPiecesOfType (theBoard b) White "Knight") == 2) || (checkPPOK whiteMovement b)==False)) then 1 else 0)
                              (if boolForBlack && checkPPOK blackMovement b 
                                  then if boolForWhite && checkPPOK whiteMovement b
                                    then if (length (allPiecesOfType (theBoard b) Black "Knight") == 2 && 
                                            length (allPiecesOfType (theBoard b) White "Knight") == 2 && 
                                            checkIfSame (fromJust whiteMovement) (fromJust blackMovement))
                                        then replace2 (replace2 (theBoard b) (checkPawnPos (getFirstCor (theBoard b)) BlackPawn 0 0) E) (checkPawnPos (getFourthCor (theBoard b)) WhitePawn 0 4) E
                                        else pawn2KnightB blackMovement (pawn2KnightW whiteMovement (theBoard b))
                                    else pawn2KnightB blackMovement (theBoard b)
                                  else if boolForWhite && checkPPOK whiteMovement b
                                    then  pawn2KnightW whiteMovement (theBoard b)
                                    else (theBoard b))
    putStrLn (show changedB)
    if checkGameEnd changedB
    then putStrLn(finallyDone changedB)                         
    else gameLoop changedB bA bAS wA wAS
  
--Prints out the messages for the round.
finallyDone :: GameState -> String
finallyDone changedBoard | (whitePen changedBoard) >= 2 && (blackPen changedBoard) < 2
                = "Winner -> Black, # of BlackPawn: " ++ [(intToDigit(length(allPiecesOfType (theBoard changedBoard) Black "Pawn")))]
                ++ "\nLoser -> White, # of WhitePawn _ PenaltyLoss" ++ [(intToDigit(length(allPiecesOfType (theBoard changedBoard) White "Pawn")))]                
finallyDone changedBoard | (blackPen changedBoard) >= 2 && (whitePen changedBoard) < 2
                = "Winner -> White, # of WhitePawn: "++ [(intToDigit(length(allPiecesOfType (theBoard changedBoard) White "Pawn")))]
                ++ "\nLoser -> Black, # of BlackPawn _ PenaltyLoss" ++ [(intToDigit(length(allPiecesOfType (theBoard changedBoard) Black "Pawn")))] 
finallyDone changedBoard | length(allPiecesOfType (theBoard changedBoard) Black "Pawn") > length(allPiecesOfType (theBoard changedBoard) White "Pawn")
                = "Winner -> Black, # of BlackPawn: " ++ [(intToDigit(length(allPiecesOfType (theBoard changedBoard) Black "Pawn")))]
                ++ "\nLoser -> White, # of WhitePawn _ NoPawnLoss" ++ [(intToDigit(length(allPiecesOfType (theBoard changedBoard) White "Pawn")))]    
finallyDone changedBoard | length(allPiecesOfType (theBoard changedBoard) Black "Pawn") <length(allPiecesOfType (theBoard changedBoard) White "Pawn")
                = "Winner -> White, # of WhitePawn: "++ [(intToDigit(length(allPiecesOfType (theBoard changedBoard) White "Pawn")))]
                ++ "\nLoser -> Black, # of BlackPawn _ NoPawnLoss" ++ [(intToDigit(length(allPiecesOfType (theBoard changedBoard) Black "Pawn")))] 
finallyDone changedBoard |  [(intToDigit(length(allPiecesOfType (theBoard changedBoard) Black "Pawn")))] == [(intToDigit(length(allPiecesOfType (theBoard changedBoard) White "Pawn")))]
                 = "No Winner!, # of WhitePawn: "++ [(intToDigit(length(allPiecesOfType (theBoard changedBoard) White "Pawn")))]
                ++ "\nDraw!, # of BlackPawn _ NoPawnDraw" ++ [(intToDigit(length(allPiecesOfType (theBoard changedBoard) Black "Pawn")))] 
         
--Checks if a pawn is on the end of the board.
checkPawnAtTheEdge :: GameState -> Chooser -> String -> Chooser -> String -> IO()
checkPawnAtTheEdge b bA bAS wA wAS = do
    blackPP <- iterCor (getFirstCor (theBoard b)) BlackPawn
    whitePP <- iterCor (getFourthCor (theBoard b)) WhitePawn
    if (blackPP || whitePP) 
        then doPP b bA bAS wA wAS blackPP whitePP
        else gameLoop b bA bAS wA wAS
         
--Upgrades a black pawn to a knight
pawn2KnightB :: Maybe [(Int, Int)] -> Board -> Board
pawn2KnightB m b | m == Nothing && length (allPiecesOfType b Black "Knight") < 2 = replace2 b (checkPawnPos (getFirstCor b) BlackPawn 0 0) BK 
                 | m /=Nothing = replace2 (replace2 b ((fromJust m) !! 0) BP) (checkPawnPos (getFirstCor b) BlackPawn 0 0) E
                 | otherwise = b

--Upgrades a white pawn to a knight
pawn2KnightW :: Maybe [(Int, Int)] -> Board -> Board
pawn2KnightW m b | m == Nothing && length (allPiecesOfType b White "Knight") < 2 = replace2 b (checkPawnPos (getFourthCor b) WhitePawn 0 4) WK
                        | m /= Nothing = replace2 (replace2 b ((fromJust m) !! 0) WP) (checkPawnPos (getFourthCor b) WhitePawn 0 4) E
                        | otherwise = b

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
