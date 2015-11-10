module Tictactoe.Att (
    playAttacker
) where

import Tictactoe.Base
import Tictactoe.HTTPHelper

attMoves :: [Coords]
attMoves = [(1,1),(2,2),(0,1),(1,0),(2,0)]

playAttacker :: String -> IO ()
playAttacker name = playAttacker' attMoves [] (TictactoeReq Attacker name BencodeDict)

playAttacker' :: [Coords] -> Board -> TictactoeReq -> IO ()
playAttacker' moves board req =
    case moves of
        [] -> putStrLn "The game is finished (should not be reachable)"
        ((x, y) : left) -> let
            boardWithMove = ((x, y, oppSign) : board)
            in do
                makeMove req boardWithMove
                case gameState boardWithMove oppSign of
                    Won -> putStrLn "You have won the game"
                    Lost -> putStrLn "You have lost the game"
                    Tie -> putStrLn "The game is tied"
                    Ongoing -> do
                        newBoard <- getMove req
                        playAttacker' left newBoard req
                        
--(1,1) -> Just ((0,0,'o'),ExpOppositeCorner (2,2))
--(2,2) -> Just ((0,2,'o'),NoExp)
--(0,1) -> Just ((2,1,'o'),NoExp)
--(1,0) -> Just ((1,2,'o'),NoExp)
--(2,0)
