module Tictactoe.Bencode.Encoder (
    stringifyBoard,
    stringifyField
) where

import Tictactoe.Base

stringifyBoard :: Board -> String
stringifyBoard board = "l" ++ (stringifyBoardFields board "") ++ "e"

stringifyBoardFields :: Board -> String -> String
stringifyBoardFields [] str = str
stringifyBoardFields (field : left) str = let
    strField = stringifyField field
    in stringifyBoardFields left (str ++ strField)

stringifyField :: BoardField -> String
stringifyField (x, y, v) = "d1:v1:" ++ [v] ++ "1:xi" ++ (show x) ++ "e1:yi" ++ (show y) ++ "ee"
