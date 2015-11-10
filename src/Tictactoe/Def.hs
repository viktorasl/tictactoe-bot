module Tictactoe.Def (
    playDefender
) where

import Tictactoe.Base
import Tictactoe.HTTPHelper
import Tictactoe.Move

playDefender :: String -> IO ()
playDefender name = playDefender' (TictactoeReq Defender name BencodeList) [ExpCenter, ExpAnyCorner]

playDefender' :: TictactoeReq -> [ExpectedMove Coords] -> IO ()
playDefender' req scens = do
    board <- getMove req
    case def scens board of
        Just (field, scen) -> do
            madeMove <- makeMove req (field : board)
            playDefender' req [scen]
        _ -> putStrLn "The game is finished"
