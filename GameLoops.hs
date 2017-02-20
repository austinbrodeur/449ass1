module GameLoops (
	startGame,
	gameLoop,
	stratsPrint,
	stratsList,
	intMode,
	checkModes
) where


import ApocTools
import ApocStrategyHuman
import ApocStrategyDefensive
--import ApocStrategyOffensive
import Data.List
import Data.Maybe (fromJust, isNothing)
import Checks
import Data.Char

-- Returns strategies
stratsPrint :: String
stratsPrint = "\nStrategies:\n  offensive\n  defensive\n  human\n"

stratsList :: [String]
stratsList = ["offensive", "defensive", "human"]

-- Interactive mode. Shows strategies and gets user input.
intMode :: IO()
intMode = do
  putStrLn ("\nEnter one of the strategies following strategies for each challenger:\n" ++ stratsPrint)
  putStrLn "Enter the black strategy: "
  blackStrat <- getLine
  putStrLn "Enter the white strategy: "
  whiteStrat <- getLine
  checkModes blackStrat whiteStrat

-- Checks if the input from either interactive mode or normal mode is correct and runs the game if it is.
checkModes :: String -> String -> IO()
checkModes bAS wAS = do
   let bA = stratOfPlayer bAS
   let wA = stratOfPlayer wAS
   if (elem (bAS) stratsList) && (elem (wAS) stratsList)
    then startGame (fst (bA)) (snd (bA)) (fst (wA)) (snd (wA))
    else putStrLn promptAgain

stratOfPlayer :: String -> (Chooser, String)
stratOfPlayer name | (name == "human") = (human, name)
                   | (name == "offensive") = (human, name)
                   | (name == "defensive") = (human, name)

              
promptAgain :: String
promptAgain = "  human\n  offensive\n  defensive\n"
{-
initializeGame :: Chooser -> String -> Chooser -> String -> IO()
initializeGame blackAI blackName whiteAI whiteName = do
  putStrLn $ "\nThe initial board:"
  print initBoard
  playRound initBoard blackAI blackName whiteAI whiteName
-}  
  
-- Dummy for main loop. Replace with main loop when ready.
startGame :: Chooser -> String -> Chooser -> String -> IO()
startGame bA bAS wA wAS = do
  print initBoard
  gameLoop initBoard bA bAS wA wAS
        
        
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

checkIfSame :: [(Int, Int)] -> [(Int, Int)] -> Bool
checkIfSame c1 c2 = (c1 !! 0) == (c2 !! 0)
    
-- ranks a black pawn to a knight
pawn2KnightB :: Maybe [(Int, Int)] -> Board -> Board
pawn2KnightB m b | m == Nothing && length (allPiecesOfType b Black "Knight") < 2 = replace2 b (checkPawnPos (getFirstCor b) BlackPawn 0 0) BK 
                 | m /=Nothing = replace2 (replace2 b ((fromJust m) !! 0) BP) (checkPawnPos (getFirstCor b) BlackPawn 0 0) E
                 | otherwise = b

-- ranks a white pawn to a knight
pawn2KnightW :: Maybe [(Int, Int)] -> Board -> Board
pawn2KnightW m b | m == Nothing && length (allPiecesOfType b White "Knight") < 2 = replace2 b (checkPawnPos (getFourthCor b) WhitePawn 0 4) WK
                        | m /= Nothing = replace2 (replace2 b ((fromJust m) !! 0) WP) (checkPawnPos (getFourthCor b) WhitePawn 0 4) E
                        | otherwise = b
        
 
checkPawnAtTheEdge :: GameState -> Chooser -> String -> Chooser -> String -> IO()
checkPawnAtTheEdge b bA bAS wA wAS = do
    blackPP <- iterCor (getFirstCor (theBoard b)) BlackPawn
    whitePP <- iterCor (getFourthCor (theBoard b)) WhitePawn
    if (blackPP || whitePP) 
        then doPP b bA bAS wA wAS blackPP whitePP
        else gameLoop b bA bAS wA wAS
        
                   
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
                
checkIfEqualPiece :: Board -> [(Int, Int)] -> [(Int, Int)] -> Cell
checkIfEqualPiece b pos1 pos2 | (t1 == t2) = E
                              | (t1 == Knight) = p1
                              | otherwise = p2
                               where p1 = getFromBoard b (pos1 !! 0)
                                     p2 = getFromBoard b (pos2 !! 0)
                                     t1 = typeOf $ pieceOf p1
                                     t2 = typeOf $ pieceOf p2

data PieceType = Knight | Pawn deriving (Eq, Show, Read)                                     
                                     
typeOf :: Piece -> PieceType
typeOf BlackKnight = Knight
typeOf WhiteKnight = Knight
typeOf BlackPawn   = Pawn
typeOf WhitePawn   = Pawn
 
   
-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)
        
        
        
    






