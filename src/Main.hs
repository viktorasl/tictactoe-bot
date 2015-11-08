module Main where

import Tictactoe.Att
import Tictactoe.Def

gameURLStr :: String
gameURLStr = "http://tictactoe.homedir.eu/game/"

main :: IO ()
main = do
    putStrLn "Game name: "  
    name <- getLine
    putStrLn "Game mode: (D | A)?"
    mode <- getLine
    case mode of
        "D" -> playDefender (gameURLStr ++ name)
        "A" -> playAttacker (gameURLStr ++ name)
        _ -> putStrLn "Game mode is unknown"
