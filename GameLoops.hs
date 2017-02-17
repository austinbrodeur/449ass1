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
--import ApocStrategyDefensive
--import ApocStrategyOffensive
import Data.List
import Data.Maybe (fromJust, isNothing)
import Checks


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
checkModes :: [Char] -> [Char] -> IO()
checkModes x y
  | ((elem x stratsList) && (elem y stratsList))  = startGame x y
  | otherwise                                     = intMode


-- Dummy for main loop. Replace with main loop when ready.
startGame :: String -> String -> IO()
startGame x y = do
                let board = initBoard
		let whitePieces = [WP, WP, WP, WP, WP, WK, WK]
		let blackPieces = [BP, BP, BP, BP, BP, BK, BK]

		gameLoop board y x whitePieces blackPieces


--if the bool is true keep going
gameLoop :: GameState -> String -> String -> PlayerPieces -> PlayerPieces -> IO()
gameLoop board pw pb wp bp = do


				white <- (if (pw == "offensive")
					     then human board Normal White
					     else if (pw == "defensive")
					             then human board Normal White
						     else human board Normal White )

				black <- (if (pb == "offensive")
					     then human board Normal Black
					     else if (pb == "defensive")
					            then human board Normal Black
						    else human board Normal Black )
				putStrLn $ show board
{-
				if (black == Nothing)
				   then blackPlay board = Passed
				   else if (fst (head (fromJust (black))) == -1)
				           then blackPlay board = Goofed ((head (tail (fromJust black))), (head (head (tail (fromJust black)))))
					   else blackPlay board = Played ((head (fromJust black)), (head (tail (fromJust black))))
				
				if (white == Nothing)
				   then let whitePlay board = Passed
				   else if (fst (head (fromJust (white))) == -1)
				           then let whitePlay board = Goofed ((head (tail (fromJust white))), (head (head (tail (fromJust white)))))
					   else let whitePlay board = Played ((head (fromJust white)), (head (tail (fromJust white))))

				if (blackPlay board == Goofed)
				   then let blackPen board += 1
			  	   else let blackPen board += 0
				
				if (whitePlay board == Goofed)
				   then whitePen board += 1
			  	   else whitePen board += 0

				if (checkValidCapture (theBoard board) (head (tail (fromJust white)))
				   then v1 <- capture ((getFromBoard board (head (fromJust white))) (getFromBoard board (head (tail (fromJust white))))
			
				if (checkValidCapture (theBoard board) (head (tail (fromJust black)))
				   then v2 <- capture ((getFromBoard board (head (fromJust black))) (getFromBoard board (head (tail (fromJust black))))

				--movements //replacing shit
		
				let winner = (checkEndGame board wp bp) 
				if (winner /= Nothing)
				   then do
					putStrLn $ show board
					putStrLn $ "Holy Fuck, " ++ winner ++ " just won!"
				   else (gameLoop board pw pb wp bp)
-}



















