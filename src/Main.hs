module Main where

import Tictactoe.Base
import Tictactoe.Bencode.Encoder
import Tictactoe.Bencode.Decoder
import Tictactoe.Move
import Tictactoe.Att
import Tictactoe.HTTPHelper

gameURLStr :: String
gameURLStr = "http://tictactoe.homedir.eu/game/"

main :: IO ()
main = do
    putStrLn "Game name: "  
    name <- getLine
    putStrLn "Game mode: (D | A)?"
    mode <- getLine
    case mode of
        "D" -> playDefender (gameURLStr ++ name)
        "A" -> playAttacker (gameURLStr ++ name)
        _ -> putStrLn "Game mode is unknown"

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
