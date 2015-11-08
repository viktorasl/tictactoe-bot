module Tictactoe.BencodeDict.Encoder (
	stringifyBoard
) where

import Tictactoe.Base
import Tictactoe.Bencode.Encoder (stringifyField)

stringifyBoard :: Board -> String
stringifyBoard board = "d" ++ (stringifyBoardFields board 0 "") ++ "e"

stringifyBoardFields :: Board -> Int -> String -> String
stringifyBoardFields [] _ str = str
stringifyBoardFields (field : left) num str = let
    strField = stringifyField field
    concatStr = (str ++ "1:" ++ (show num) ++ strField)
    in stringifyBoardFields left (num + 1) concatStr
