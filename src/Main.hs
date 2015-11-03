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
    resp <- simpleHTTP (postRequestWithBody (gameURLStr ++ "player/1") "application/bencode+list" (stringifyBoard [(1, 1, 'x')])) >>= getResponseBody
    putStrLn resp
    waitForMove [ExpCenter, ExpAnyCorner]

mockResp :: IO String
mockResp = do
    return "ld1:v1:x1:xi1e1:yi1eee"

waitForMove :: [ExpectedMove Coords] -> IO ()
waitForMove scens = do
    --response <- mockResp
    response <- getMove (gameURLStr ++ "player/2")
    case parseBoard response of
        board -> 
            case def scens board of
                Just (field, scen) -> do
                    madeMove <- makeMove (gameURLStr ++ "player/2") (field : board)
                    putStrLn madeMove
                    waitForMove [scen]
                _ -> putStrLn "The game is finished"
