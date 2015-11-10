module Tictactoe.HTTPHelper (
    getMove,
    makeMove,
    TictactoePlayer(Attacker,Defender),
    TictactoeCType(BencodeList,BencodeDict),
    TictactoeReq(TictactoeReq)
) where

import Network.HTTP
import Network.URI
import Network.BufferType
import Tictactoe.Base
import Tictactoe.Bencode.Encoder as BencodeList
import Tictactoe.Bencode.Decoder as BencodeList
import Tictactoe.BencodeDict.Encoder as BencodeDict
import Tictactoe.BencodeDict.Decoder as BencodeDict

data TictactoePlayer = Attacker | Defender
instance Show TictactoePlayer where
  show Attacker = "1"
  show Defender = "2"

data TictactoeCType = BencodeList | BencodeDict
instance Show TictactoeCType where
  show BencodeList = "application/bencode+list"
  show BencodeDict = "application/bencode+map"

data TictactoeReq = TictactoeReq {
    player :: TictactoePlayer
  , gameName :: String
  , contentType :: TictactoeCType
}

gameHost :: String
gameHost = "http://tictactoe.homedir.eu/game"

fullUrl :: TictactoeReq -> String
fullUrl req = gameHost ++ "/" ++ (gameName req) ++ "/player/" ++ (show (player req))

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

boardifyString :: String -> TictactoeCType -> Board
boardifyString strBoard cType =
  case cType of
    BencodeList -> BencodeList.parseBoard strBoard
    BencodeDict -> BencodeDict.parseBoard strBoard

strBoard :: Board -> TictactoeCType -> String
strBoard board cType =
  case cType of
    BencodeDict -> BencodeDict.stringifyBoard board
    BencodeList -> BencodeList.stringifyBoard board

getMove :: TictactoeReq -> IO Board
getMove req = do
  resp <- simpleHTTP (getMoveRequestString (fullUrl req) (show (contentType req))) >>= getResponseBody
  return $ boardifyString resp (contentType req)

makeMove :: TictactoeReq -> Board -> IO String
makeMove req board = simpleHTTP (postRequestWithBody (fullUrl req) (show (contentType req)) (strBoard board (contentType req))) >>= getResponseBody
