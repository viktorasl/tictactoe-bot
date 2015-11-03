module Tictactoe.HTTPHelper where

import Tictactoe.Base
import Tictactoe.Encoder

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

getMove :: String -> IO String
getMove url = simpleHTTP (getMoveRequestString url) >>= getResponseBody

makeMove :: String -> Board -> IO String
makeMove url board = simpleHTTP (postRequestWithBody url "application/bencode+list" (stringifyBoard board)) >>= getResponseBody
