{- | This module is used for CPSC 449 for the Apocalypse assignment.
This is merely a skeleton to get you started.  It has VERY little functionality.
Copyright: Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
Permission to use, copy, modify, distribute and sell this software
and its documentation for any purpose is hereby granted without fee, provided
that the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation. The University of Calgary makes no representations about the
suitability of this software for any purpose. It is provided "as is" without
express or implied warranty.
-}

module ApocStrategyHuman where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.IO.Unsafe
import ApocTools

{- | The human strategy only prompts the user for input. There is no validity
     checking on this end as that is handled by the move validity checker in
     Apoc.hs
-}
human    :: Chooser
human b Normal        c = do
    if c == Black 
    then putStrLn $ "Enter the move coordinates for player Black in the form \'srcX srcY destX destY\'"
                    ++" n(0 >= n >= 4, or just enter return for a \'pass\') B2:"
    else putStrLn $ "Enter the move coordinates for player White in the form \'srcX srcY destX destY\'"
                    ++" n(0 >= n >= 4, or just enter return for a \'pass\') W2:"
    line <- getLine
    putStrLn line
    let v = convertToIntList line
    if length v == 0 then return Nothing else return (Just [((v !! 0), (v !! 1)), ((v !! 2), (v !! 3))])
human b PawnPlacement c = do
    if c == Black 
    then putStrLn $ "Enter the coordinates to place the pawn for player Black in the form \'destX destY\':"
                    ++" [0 >= n >= 4] B1:"
    else putStrLn $ "Enter the coordinates to place the pawn for player White in the form \'destX destY\':"
                    ++" [0 >= n >= 4] W1:"
    line <- getLine
    putStrLn line
    let v = convertToIntList line
    if length v == 0 then return Nothing else return (Just [((v !! 0), (v !! 1))])

{- | A helper function for converting user read input from stdin into a list of
     Ints that can be used as valid coordinates.
-}
convertToIntList :: String -> [Int]
convertToIntList = map read . words