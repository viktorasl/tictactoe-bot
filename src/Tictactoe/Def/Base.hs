module Tictactoe.Def.Base (
    playDefender
) where

import Tictactoe.Base
import Tictactoe.HTTPHelper
import Tictactoe.Def.Move

playDefender :: String -> IO ()
playDefender name = playDefender' (TictactoeReq Defender name BencodeList) [ExpCenter, ExpAnyCorner]

playDefender' :: TictactoeReq -> [ExpectedMove Coords] -> IO ()
playDefender' req scens = do
    board <- getMove req
    case gameState board oppSign of
        Won -> putStrLn "You have won the game"
        Lost -> putStrLn "You have lost the game"
        Tie -> putStrLn "The game is tied"
        Ongoing -> do
            case def scens board of
                Just (field, scen) -> do
                    madeMove <- makeMove req (field : board)
                    playDefender' req [scen]
                _ -> putStrLn "The game is finished (should not be reachable)"
