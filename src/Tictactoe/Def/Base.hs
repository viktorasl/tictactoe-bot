module Tictactoe.Def.Base (
    playDefender
) where

import Tictactoe.Base
import Tictactoe.HTTPHelper
import Tictactoe.Def.Move

playDefender :: String -> IO ()
playDefender name = playDefender' (TictactoeReq Defender name BencodeList) First

playDefender' :: TictactoeReq -> ExpectedMove Coords -> IO ()
playDefender' req exp = do
    board <- getMove req
    case gameState board oppSign of
        Won -> putStrLn "You have won the game"
        Lost -> putStrLn "You have lost the game"
        Tie -> putStrLn "The game is tied"
        Ongoing -> do
            case def exp board of
                Just (field, scen) -> do
                    madeMove <- makeMove req (field : board)
                    playDefender' req exp
                _ -> putStrLn "The game is finished (should not be reachable)"
