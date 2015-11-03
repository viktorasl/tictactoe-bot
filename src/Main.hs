module Main where

import Tictactoe.Encoder
import Tictactoe.Base
import Tictactoe.Move
import Tictactoe.Decoder
import Tictactoe.Att
import Tictactoe.HTTPHelper

gameURLStr :: String
gameURLStr = "http://tictactoe.homedir.eu/game/randomgamenamex13/"

main :: IO ()
main = do
    --playAttacker gameURLStr
    --playAttacker gameURLStr
    putStrLn "resp"

playDefender :: String -> IO ()
playDefender url = playDefender' url [ExpCenter, ExpAnyCorner]

playDefender' :: String -> [ExpectedMove Coords] -> IO ()
playDefender' url scens = do
    board <- getMove (url ++ "player/2")
    case def scens board of
        Just (field, scen) -> do
            madeMove <- makeMove (url ++ "player/2") (field : board)
            playDefender' url [scen]
        _ -> putStrLn "The game is finished"
