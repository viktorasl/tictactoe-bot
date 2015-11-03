module Tictactoe.HTTPHelper (
    getMove,
    makeMove
) where

import Tictactoe.Base
import Tictactoe.Encoder
import Tictactoe.Decoder

import Network.HTTP
import Network.URI
import Network.BufferType

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

getMove :: String -> IO Board
getMove url = do
    resp <- simpleHTTP (getMoveRequestString url) >>= getResponseBody
    return $ parseBoard resp

makeMove :: String -> Board -> IO String
makeMove url board = simpleHTTP (postRequestWithBody url "application/bencode+list" (stringifyBoard board)) >>= getResponseBody
