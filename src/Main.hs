module Main where

import Tictactoe.Encoder
import Tictactoe.Base
import Tictactoe.Move
import Tictactoe.Decoder
import Tictactoe.Att
import Tictactoe.HTTPHelper

import Network.HTTP

gameURLStr :: String
gameURLStr = "http://tictactoe.homedir.eu/game/randomgamenamex13/"

main :: IO ()
main = do
    --playAttacker gameURLStr
    putStrLn "resp"

mockResp :: IO String
mockResp = do
    return "ld1:v1:x1:xi1e1:yi1eee"

playDef :: IO ()
playDef = playDef' [ExpCenter, ExpAnyCorner]

playDef' :: [ExpectedMove Coords] -> IO ()
playDef' scens = do
    board <- getMove (gameURLStr ++ "player/2")
    case def scens board of
        Just (field, scen) -> do
            madeMove <- makeMove (gameURLStr ++ "player/2") (field : board)
            playDef' [scen]
        _ -> putStrLn "The game is finished"
