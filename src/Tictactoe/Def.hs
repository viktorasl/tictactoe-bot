module Tictactoe.Def (
    playDefender
) where

import Tictactoe.Base
import Tictactoe.Bencode.Encoder
import Tictactoe.Bencode.Decoder
import Tictactoe.HTTPHelper
import Tictactoe.Move

playDefender :: String -> IO ()
playDefender url = playDefender' url [ExpCenter, ExpAnyCorner]

playDefender' :: String -> [ExpectedMove Coords] -> IO ()
playDefender' url scens = do
    board <- getMove (url ++ "/player/2") "application/bencode+list"
    let
        parsedBoard = parseBoard board
        in case def scens parsedBoard of
            Just (field, scen) -> do
                madeMove <- makeMove (url ++ "/player/2") "application/bencode+list" (stringifyBoard (field : parsedBoard))
                playDefender' url [scen]
            _ -> putStrLn "The game is finished"
