module Tictactoe.HTTPHelper (
    getMove,
    makeMove
) where

import Network.HTTP
import Network.URI
import Network.BufferType

toBufOps :: BufferType a => Request a -> BufferOp a
toBufOps _ = bufferOps

getMoveRequest :: BufferType ty => URI -> String -> Request ty
getMoveRequest uri acceptType =
    Request { rqURI      = uri
            , rqBody     = buf_empty (bufferOps)
            , rqHeaders  = [ Header HdrContentLength "0"
                           , Header HdrUserAgent     defaultUserAgent
                           , Header HdrAccept        acceptType
                           ]
            , rqMethod   = GET
            }

getMoveRequestString :: String -> String -> Request_String
getMoveRequestString urlString acceptType =
    case parseURI urlString of
        Nothing -> error ("getRequest: Not a valid URL - " ++ urlString)
        Just uri  -> getMoveRequest uri acceptType

getMove :: String -> String -> IO String
getMove url acceptType = simpleHTTP (getMoveRequestString url acceptType) >>= getResponseBody

makeMove :: String -> String -> String -> IO String
makeMove url contentType strBoard = simpleHTTP (postRequestWithBody url contentType strBoard) >>= getResponseBody
