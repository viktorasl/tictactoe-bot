module Tictactoe.Base
where
import Data.Maybe

type Coords = (Int, Int)
type BoardField = (Int, Int, Char)
type Board = [BoardField]

mySign = 'o'
oppSign = 'x'

fieldExists :: Board -> Coords -> Maybe BoardField
fieldExists board coords = listToMaybe $ filter (\field' -> coordsEqual coords field') board

coordsEqual :: Coords -> BoardField -> Bool
coordsEqual (x1, y1) (x2, y2, _) = if (x1 == x2 && y1 == y2) then True else False 
