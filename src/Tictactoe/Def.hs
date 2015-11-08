module Tictactoe.Def (
    playDefender
) where

import Tictactoe.Base
import Tictactoe.HTTPHelper
import Tictactoe.Move

playDefender :: String -> IO ()
playDefender url = playDefender' url [ExpCenter, ExpAnyCorner]

playDefender' :: String -> [ExpectedMove Coords] -> IO ()
playDefender' url scens = do
    board <- getMove (url ++ "/player/2")
    case def scens board of
        Just (field, scen) -> do
            madeMove <- makeMove (url ++ "/player/2") (field : board)
            playDefender' url [scen]
        _ -> putStrLn "The game is finished"
