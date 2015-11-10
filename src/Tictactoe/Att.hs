module Tictactoe.Att (
    playAttacker
) where

import Tictactoe.Base
import Tictactoe.HTTPHelper
import Tictactoe.Att.Move

playAttacker :: String -> IO ()
playAttacker name = playAttacker' ((1, 1, oppSign), First) [] (TictactoeReq Attacker name BencodeDict)

playAttacker' :: (BoardField, ExpectedMove Coords Coords) -> Board -> TictactoeReq -> IO ()
playAttacker' (move, exp) board req = let
    boardWithMove = (move : board)
    in do
        makeMove req boardWithMove
        case gameState boardWithMove oppSign of
            Won -> putStrLn "You have won the game"
            Lost -> putStrLn "You have lost the game"
            Tie -> putStrLn "The game is tied"
            Ongoing -> do
                newBoard <- getMove req
                case att exp newBoard of
                    Just moveExp -> playAttacker' moveExp newBoard req 
                    _ -> putStrLn "The game is finished (should not be reachable"
                        