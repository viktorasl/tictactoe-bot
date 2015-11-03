module Tictactoe.Att (playAttacker) where

import Tictactoe.Base

attMoves :: [Coords]
attMoves = [(1,1),(2,2),(0,1),(1,0),(2,0)]

playAttacker :: String -> IO ()
playAttacker url = playAttacker' attMoves [] url

playAttacker' :: [Coords] -> Board -> String -> IO ()
playAttacker' moves board url =
    case moves of
        [] -> putStrLn "The game is finished"
        (move : left) -> do
            putStrLn "Move"

--(1,1) -> Just ((0,0,'o'),ExpOppositeCorner (2,2))
--(2,2) -> Just ((0,2,'o'),NoExp)
--(0,1) -> Just ((2,1,'o'),NoExp)
--(1,0) -> Just ((1,2,'o'),NoExp)
--(2,0)

--resp <- simpleHTTP (postRequestWithBody (url ++ "/player/1") "application/bencode+list" (stringifyBoard [(1, 1, 'x')])) >>= getResponseBody