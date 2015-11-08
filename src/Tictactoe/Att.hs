module Tictactoe.Att (
	playAttacker
) where

import Tictactoe.Base
import Tictactoe.HTTPHelper

attMoves :: [Coords]
attMoves = [(1,1),(2,2),(0,1),(1,0),(2,0)]

playAttacker :: String -> IO ()
playAttacker url = playAttacker' attMoves [] url

playAttacker' :: [Coords] -> Board -> String -> IO ()
playAttacker' moves board url =
    case moves of
        [] -> putStrLn "The game is finished"
        ((x, y) : left) -> do
            makeMove (url ++ "/player/1") ((x, y, oppSign) : board)
            newBoard <- getMove (url ++ "/player/1")
            playAttacker' left newBoard url

--(1,1) -> Just ((0,0,'o'),ExpOppositeCorner (2,2))
--(2,2) -> Just ((0,2,'o'),NoExp)
--(0,1) -> Just ((2,1,'o'),NoExp)
--(1,0) -> Just ((1,2,'o'),NoExp)
--(2,0)
