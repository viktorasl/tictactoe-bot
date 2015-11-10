module Main where

import Tictactoe.Att
import Tictactoe.Def.Base

main :: IO ()
main = do
    putStrLn "Game name: "  
    name <- getLine
    putStrLn "Game mode: (D | A)?"
    mode <- getLine
    case mode of
        "D" -> playDefender name
        "A" -> playAttacker name
        _ -> putStrLn "Game mode is unknown"
