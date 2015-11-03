module Main where

import Tictactoe.Encoder
import Tictactoe.Base
import Tictactoe.Move
import Tictactoe.Decoder
import Tictactoe.Att

import Network.HTTP
import Network.URI
import Network.BufferType
import Network.HTTP.Base

gameURLStr :: String
gameURLStr = "http://tictactoe.homedir.eu/game/randomgamenamex13/"

main :: IO ()
main = do
    playAttacker gameURLStr
    --resp <- simpleHTTP (postRequestWithBody (gameURLStr ++ "player/1") "application/bencode+list" (stringifyBoard [(1, 1, 'x')])) >>= getResponseBody
    --putStrLn resp
    --waitForMove [ExpCenter, ExpAnyCorner]

mockResp :: IO String
mockResp = do
    return "ld1:v1:x1:xi1e1:yi1eee"

toBufOps :: BufferType a => Request a -> BufferOp a
toBufOps _ = bufferOps

getMoveRequest :: BufferType ty => URI -> Request ty
getMoveRequest uri =
    Request { rqURI      = uri
            , rqBody     = buf_empty (bufferOps)
            , rqHeaders  = [ Header HdrContentLength "0"
                           , Header HdrUserAgent     defaultUserAgent
                           , Header HdrAccept        "application/bencode+list"
                           ]
            , rqMethod   = GET
            }

getMoveRequestString :: String -> Request_String
getMoveRequestString urlString =
    case parseURI urlString of
        Nothing -> error ("getRequest: Not a valid URL - " ++ urlString)
        Just uri  -> getMoveRequest uri

getMove :: String -> IO String
getMove url = simpleHTTP (getMoveRequestString url) >>= getResponseBody

makeMove :: String -> Board -> IO String
makeMove url board = simpleHTTP (postRequestWithBody url "application/bencode+list" (stringifyBoard board)) >>= getResponseBody

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
