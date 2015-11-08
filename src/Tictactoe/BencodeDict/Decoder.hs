module Tictactoe.BencodeDict.Decoder (
    parseBoard
) where

import Tictactoe.Base
import Tictactoe.Bencode.Decoder (parseBoardField', defaultField)
import Data.Char

parseBoard :: String -> Board
parseBoard ('d' : list) = parseBoardFields' list []

parseBoardFields' :: String -> Board -> Board
parseBoardFields' ('e' : dict) board = board
parseBoardFields' ('1' : dict) board =
    let
    (field, rest) = parseBoardField' (drop 3 dict) defaultField
    in parseBoardFields' rest (field : board)
