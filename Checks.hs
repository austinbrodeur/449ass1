--PLEASE NOTE: None of this is guaranteed to work :33

module Checks (
    PlayerPieces,
    knightValid,
    aggroPawnValid,
    passivePawnValid,
    capture,
    checkPromotion,
    promoMoveValid,
    count,
    checkEndGame,
    allPawnsCaptured,
    twoPenalties,
    checkBothPass,
    checkIfEmpty,
    checkIfOwned,
    bothPass, splitInts, checkLen, greaterFour, lesserZero, checkGreater, checkLesser) where

import ApocTools

type PlayerPieces = [Cell]

whiteGoons = [WP, WK]
blackGoons = [BP, BK]

--Offsets for knight and pawn
knightOffset = [(1,2), (1,-2), (-1,2), (-1,-2), (2,1), (-2,1), (2,-1), (-2,-1)]
aggroPawnOffset = [(1, 1), (-1, 1)]

--Checks if chosen cell is an empty
checkIfEmpty :: Board -> Int -> Int -> Bool
checkIfEmpty b x y = if (getFromBoard b (x, y) == E)
			then True
			else False

--Checks if chosen piece is theirs
checkIfOwned :: Board -> Player -> Int -> Int -> Bool
checkIfOwned b p x y | p == White = if (getFromBoard b (x, y) `elem` whiteGoons)
					    then True
					    else False
		     | otherwise       = if (getFromBoard b (x, y) `elem` blackGoons)
					    then True
				   	    else False

--Checks if knight movement is valid
knightValid :: [Int] -> Bool
knightValid (a:b:c:d:s) = elem ((a - c), (b - d)) knightOffset

--Checks if pawn movement is valid when capturing
aggroPawnValid :: Board -> [Int] -> Bool
aggroPawnValid board (a:b:c:d:s) = (elem ((a - c), (b - d)) aggroPawnOffset) && checkValidCapture board (c, d)

--Checks if the cell to capture is not empty
checkValidCapture :: Board -> (Int, Int) -> Bool
checkValidCapture b op = ((getFromBoard b op) /= E)

--Checks if pawn movement is valid when not capturing
passivePawnValid :: [Int] -> Bool
passivePawnValid (a:b:c:d:s) = (((a - c), (b - d)) == (0, 1))

{-Determines the outcome of a clash.
 -PARAMETER the cell type of the white player.
 -PARAMETER the cell type of the black player.
 -RETURNS the cell type that should be in the contested position afterwards. -}
capture :: Cell -> Cell -> Cell
capture t1 t2 | (t1 == WK) = if (t2 == BK)
                                     then E
                                     else WK
                   | (t1 == WP) = if (t2 == BK)
                                     then BK
                                     else E

{-When a pawn reaches the other side, checks if the pawn can be promoted.
 -PARAMETER a player so we know the color
 -PARAMETER the list of all a player's active pieces
 -RETURNS whether or not a pawn can be promoted.
 -If this function returns true, promote the pawn in question
 -else if this function returns false, allow the pawn to be placed anywhere -}
checkPromotion :: Player -> PlayerPieces -> Bool
checkPromotion p cs | p == White = if ((count WK cs) >= 2)
				      then False
				      else True
		    | p == Black = if ((count BK cs) >= 2)
				      then False
				      else True

{-Checks if the player input after reaching other side (without promotion) is valid.
 PARAMETER the board
 PARAMETER the xy coordinates
 RETURNS True or False
 If true, then the pawn can be moved to the new location.
 If false, prompt for new xy or punish? -}
promoMoveValid :: Board -> (Int, Int) -> Bool
promoMoveValid b p | ((b !! fst p) !! snd p) == E = True
		   | otherwise = False

--Auxiliary function for counting occurences of something in a list. Can be used with anything.
count :: Eq a => a -> [a] -> Int
count x [] = 0
count x (b:bs) | x == b    = 1 + count x bs
	       | otherwise = count x bs


{- Methods used by both AI strategies to determine moves
-}
-- | Choose a random move from a list. Moves are weighted towards the front of the list 
chooseRandomMove :: [a] -> Float -> Maybe a
chooseRandomMove [] f = Nothing
chooseRandomMove (x:[]) f = Just x
chooseRandomMove (x:xs) f | rand <= f = Just x
                            | otherwise chooseRandomMove xs f
  			  where rand = unsafePerformIO (random IO :: IO Float)  
 			  
-- | Choose a random move from a list		  
chooseMove :: [a] -> Maybe a
chooseMove [] = Nothing
chooseMove xs = Just (xs !! index)
                   where index = floor ((unsafePerformIO (randomIO :: IO Float)) * (fromIntegral (length xs)))
		  
-- |Returns whether a position is safe or not
safeTile :: Board -> Player -> (Int, Int) -> Bool
safeTile b player p = tileSafeFromCapture (allPlayerMoves b (otherPlayer player)) p

-- |Returns whether a position is safe from being captured
tileSafeFromCapture :: [(Int, Int)] -> (Int, Int) -> Bool
tileSafeFromcapture [] pos = True
tileSafeFromCapture (m:ms) pos | (m == pos) = False
                                   | otherwise = tileSafeFromCapture ms pos
 				   
-- |Returns all possible moves
allPossibleMoves :: Board -> Player -> [(Int, Int)]
allPossibleMoves board player = allMoves board (allPiecesOfType board player Knight) ++ allMoves board (allPiecesOfType board player Pawn)
 
-- |Returns a list of all possible moves from a specific position
allMoves :: Board -> [(Int, Int)] -> [(Int, Int)]
allMoves board [] = []
allMoves board (p:ps) = getMoves2 board p False ++ allMoves board ps
  
-- |Returns a list of all pieces on the board
allPieces :: Board -> Player -> [(Int, Int)]
allPieces b p = (allPiecesOfType b p Knight) ++ (allPiecesOfType b p Pawn)
  
-- |Returns a list of all the specified player's pieces on the board 
allPiecesOfType :: Board -> Player -> PieceType -> [(Int, Int)]
allPiecesOfType b p t | (p == White && t == Knight) = pieces b WK 0 0 0 0 4 4
                      | (p == White && t == Pawn)   = pieces b WP 0 0 0 0 4 4
                      | (p == Black && t == Knight) = pieces b BK 0 0 0 0 4 4
                      | (p == Black && t == Pawn)   = pieces b BP 0 0 0 0 4 4
  
-- |Returns a list of all the pieces on the board of a specific cell type
pieces :: Board -> Cell -> Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
pieces b p x y minx miny maxx maxy | (x == maxx && y == maxy) = if (getFromBoard b (maxx, maxy)) == p then (x, y) : [] else []
                                   | (x == maxx) = if (getFromBoard b (maxx, y)) == p then (maxx, y) : pieces b p minx (y + 1) minx miny maxx maxy else pieces b p minx (y + 1) minx miny maxx maxy
                                   | otherwise = if (getFromBoard b (x, y)) == p then (x, y) : pieces b p (x + 1) y minx miny maxx maxy else pieces b p (x + 1) y minx miny maxx maxy
  
-- |Returns a list of moves from a piece, no capture
getMoves :: Board -> (Int, Int) -> Bool -> [(Int, Int)]
getMoves b pos valid | (piece == Knight) = moves b knightMoves pos (length knightMoves) valid
                     | (piece == Pawn)   = moves b pawnMoves pos (length pawnMoves) valid
                     where piece = typeOf (pieceOf (getFromBoard b pos))
                           player = playerOf (pieceOf (getFromBoard b pos))
                           knightMoves = getKnightMoves
                           pawnMoves = getPawnMoves player False
  
-- |Returns a list of moves from a piece, capture
getMoves2 :: Board -> (Int, Int) -> Bool -> [(Int, Int)]
getMoves2 b pos valid | (piece == Knight) = moves b knightMoves pos (length knightMoves) valid
                      | (piece == Pawn)   = moves b pawnMoves pos (length pawnMoves) valid
                      where piece = typeOf (pieceOf (getFromBoard b pos))
                            player = playerOf (pieceOf (getFromBoard b pos))
                            knightMoves = getKnightMoves
                            pawnMoves = getPawnMoves player True
 
-- | Moves a piece can make
moves :: Board -> [(Int, Int)] -> (Int, Int) -> Int -> Bool -> [(Int, Int)]
moves b m p 0 valid = []
moves b (m:ms) pos index valid | (valid && isInBounds pos && isInBounds to && getFromBoard b to == E) = to : (moves b ms pos (index - 1) valid)
                               | ((not valid) && isInBounds pos && isInBounds to) = to : (moves b ms pos (index - 1) valid)
                               | otherwise = moves b ms pos (index - 1) valid
                               where to = (fst pos + fst m, snd pos + snd m)
  			       
-- |Check if the move is within the bounds of the board
isInBounds      :: (Int, Int) -> Bool
isInBounds move | a>4 = False
		| b>4 = False
                | a<0 = False
                | b<0 = False
                | otherwise = True
                where a = fst move
                       b = snd move



--Victory/Loss conditions

{-This is not finished
 - This function will do all the checks for victory conditions.
 - First it checks if all the pawns are captured. If that condition it not met
 - it will check for two penalties from either player. If that condition if not
 - met it will check if both players passed the turn. If none of the conditions
 - are met, then the function returns Nothing, and the game continues.
 -
 - THE FATAL FLAW OF THIS FUNCTION IS THAT IF BOTH PLAYERS PASS, AND THE NUMBER
 - OF PAWNS IS THE SAME, WHITE LOSES. I don't know how to fix this because Haskell.
 -
 - ^^ I'm not sure if the above will ever even happen, but the only case where this
 - function makes no sense is if no other victory conditions are met, and both players
 - pass which having the SAME NUMBER of pawns. Only then will the answer be wrong.
 -
 - It makes sense to tie in this scenario but he does not state that in the assignment.
 -
 - RETURNS Maybe, the winner of the match. If it returns Nothing the game continues. -}
checkEndGame :: GameState -> PlayerPieces -> PlayerPieces -> Maybe Player
checkEndGame b whitePieces blackPieces = let winner = allPawnsCaptured whitePieces blackPieces in
	       if (winner == Nothing)
	       	  then let winner = twoPenalties (whitePen b) (blackPen b) in
		       if (winner == Nothing)
			  then checkBothPass whitePieces blackPieces (whitePlay b) (blackPlay b)
			  else winner
		  else winner

--First victory condition: Returns the winner, or nothing if both still have pawns.
allPawnsCaptured :: PlayerPieces -> PlayerPieces -> Maybe Player
allPawnsCaptured w b | (elem WP w) && (not (elem BP b)) = Just White
		     | (elem BP b) && (not (elem WP w)) = Just Black
		     | otherwise = Nothing

--Second victory condition: Returns the winner, or nothing if neither have 2 penalties.
twoPenalties :: Int -> Int -> Maybe Player
twoPenalties w b | w >= 2 = Just Black
		 | b >= 2 = Just White
		 | otherwise = Nothing

--Third victory condition: Returns the winner, or nothing if both don't pass.
checkBothPass :: PlayerPieces -> PlayerPieces -> Played -> Played -> Maybe Player
checkBothPass whitePieces blackPieces p1 p2 | (p1 == Passed) && (p2 == Passed) = Just (bothPass whitePieces blackPieces)
		    | otherwise = Nothing

--THIS IS THE "broken" FUNCTION
--Returns the winner always. WHITE WILL ALWAYS LOSE WHEN THEY TIE.
bothPass :: PlayerPieces -> PlayerPieces -> Player
bothPass w b | count WP w > count BP b = White
	     | count WP w < count BP b = Black
	     | otherwise = White

----------------------------------------------------------------------------------------------------


-- Gets input for the human turn and splits to input into ints. Used to do checkLen.
splitInts :: IO [Int]
splitInts = fmap (map read.words) getLine

-- Checks to make sure the user for the human strategy enters in four integer arguments to move. Will reprompt with an error message if they enter invalid input.
checkLen :: IO [Int]
checkLen = do
  ints <- splitInts
  if length ints == 4 then return ints else do
    putStrLn ("Too many or too few integers. Please enter four integers separated by spaces.")
    checkLen

-- Used in checkGreater to check if any of the values are greater than 4
greaterFour :: Ord a => Num a => a -> Bool
greaterFour x = (x <= 4)

-- Used in checkLesser to check if any of the values are less than zero
lesserZero :: Ord a => Num a => a -> Bool
lesserZero x = (x >= 0)

-- Checks if any values in the input list are greater than 4. Returns True if there is a value out of range.
checkGreater :: Ord a => Num a => [a] -> Bool
checkGreater [] = False
checkGreater (x:xs)
  | greaterFour x = checkGreater xs
  | otherwise     = True

-- Checks if any values in the input list are less than 4. Returns True if there is a value out of range.
checkLesser :: Ord a => Num a => [a] -> Bool
checkLesser [] = False
checkLesser (x:xs)
  | lesserZero x = checkLesser xs
  | otherwise    = True




