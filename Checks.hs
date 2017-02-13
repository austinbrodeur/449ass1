--PLEASE NOTE: None of this is guaranteed to work :33

module Checks (
    PlayerPieces,
    knightValid,
    aggroPawnValid,
    passivePawnValid,
    checkcapture,
    checkPromotion,
    promoMoveValid,
    count,
    checkEndGame,
    allPawnsCaptured,
    twoPenalties,
    checkBothPass,
    bothPass) where


type PlayerPieces = [Cell]



--Offsets for knight and pawn
knightOffset = [(1,2), (1,-2), (-1,2), (-1,-2), (2,1), (-2,1), (2,-1), (-2,-1)]
aggroPawnOffset = [(1, 1), (-1, 1)]

--Checks if knight movement is valid
knightValid :: (Int, Int) -> (Int, Int) -> Bool
knightValid stt des = elem ((fst stt - fst des), (snd stt - snd des)) knightOffset

--Checks if pawn movement is valid when capturing
aggroPawnValid :: (Int, Int) -> (Int, Int) -> Bool
aggroPawnValid stt des = elem ((fst stt - fst des), (snd stt - snd des)) aggroPawnOffset

--Checks if pawn movement is valid when not capturing
passivePawnValid :: (Int, Int) -> (Int, Int) -> Bool
passivePawnValid stt des = (((fst stt - fst des), (snd stt - snd des)) == (0, 1))

{-Determines the outcome of a clash.
 -PARAMETER the cell type of the white player.
 -PARAMETER the cell type of the black player.
 -RETURNS the cell type that should be in the contested position afterwards. -}
checkCapture :: Cell -> Cell -> Cell
checkCapture t1 t2 | (t1 == WK) = if (t2 == BK)
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
checkEndGame :: Maybe Player
checkEndGame = let winner = allPawnsCaptured white black
		   if (winner == Nothing)
	              then let winner = twoPenalties whitePen blackPen
			       if (winner == Nothing)
				  then checkBothPass
				  else winner
		      else winner

--First victory condition: Returns the winner, or nothing if both still have pawns.
allPawnsCaptured :: PlayerPieces -> PlayerPieces -> Maybe Player
allPawnsCaptured w b | elem WP w && not elem BP b = Just White
		     | elem BP b && not elem WP w = Just Black
		     | otherwise = Nothing

--Second victory condition: Returns the winner, or nothing if neither have 2 penalties.
twoPenalties :: Int -> Int -> Maybe Player
twoPenalties w b | w >= 2 = Just Black
		 | b >= 2 = Just White
		 | otherwise = Nothing

--Third victory condition: Returns the winner, or nothing if both don't pass.
checkBothPass :: Played -> Played -> Maybe Player
checkBothpass p1 p2 | (p1 == Pass) && (p2 == Pass) = Just bothPass white black
		    | otherwise = Nothing

--THIS IS THE "broken" FUNCTION
--Returns the winner always. WHITE WILL ALWAYS LOSE WHEN THEY TIE.
bothPass :: PlayerPieces -> PlayerPieces -> Player
bothPass w b | count WP w > count BP b = White
	     | count WP w < count BP b = Black
	     | otherwise = White

