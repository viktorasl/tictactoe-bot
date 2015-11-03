module Tictactoe.Encoder
where
import Tictactoe.Base

stringifyBoard :: Board -> String
stringifyBoard board = "l" ++ (stringifyBoardFields board "") ++ "e"

stringifyBoardFields :: Board -> String -> String
stringifyBoardFields board str =
    case board of
        [] -> str
        (field : left) -> stringifyBoardFields left (str ++ (stringifyField field))

stringifyField :: BoardField -> String
stringifyField (x, y, v) = "d1:v1:" ++ [v] ++ "1:xi" ++ (show x) ++ "e1:yi" ++ (show y) ++ "ee"
