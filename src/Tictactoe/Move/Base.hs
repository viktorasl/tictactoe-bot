module Tictactoe.Move.Base
where
import Data.Maybe
import Tictactoe.Base

finishingWin :: Board -> Char -> Maybe Coords
finishingWin board sign = finishingMove board sign

finishingBlock :: Board -> Char -> Maybe Coords
finishingBlock board sign = finishingMove board sign

finishingMove :: Board -> Char -> Maybe Coords
finishingMove board sign = let
    infos = rowsInfos board sign
    satInfos = filter (\info -> ((matchSign info) == 2) && ((length (free info)) == 1)) infos
    in listToMaybe $ map (\info -> head (free info)) satInfos

takenAnyEdge :: Board -> Maybe Coords
takenAnyEdge board = listToMaybe $ filter (\coords' -> isJust (fieldExists board coords')) [(0, 1), (1, 0), (1, 2), (2, 1)]

takenAnyCorner :: Board -> Maybe Coords
takenAnyCorner board = listToMaybe $ filter (\coords' -> isJust (fieldExists board coords')) [(0, 0), (0, 2), (2, 0), (2, 2)]

oppositeCorner :: Coords -> Coords
oppositeCorner (x, y) = (abs (x - 2), abs (y - 2))

takeCenter :: Board -> Maybe Coords
takeCenter board =
    case (fieldExists board (1, 1)) of
        Just _ -> Nothing
        Nothing -> Just (1, 1)

takeAnyEmptyCorner :: Board -> Maybe Coords
takeAnyEmptyCorner board = listToMaybe $ filter (\coords' -> isNothing (fieldExists board coords')) [(0, 0), (0, 2), (2, 0), (2, 2)]

takeAnyEmptyEdge :: Board -> Maybe Coords
takeAnyEmptyEdge board = listToMaybe $ filter (\coords' -> isNothing (fieldExists board coords')) [(0, 1), (1, 0), (1, 2), (2, 1)]
