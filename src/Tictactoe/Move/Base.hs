module Tictactoe.Move.Base
where
import Data.Maybe
import Tictactoe.Base

defaultMove :: Board -> Char -> Char -> Maybe BoardField
defaultMove board mySign oppSign =
    case finishingMove board mySign of
        Just (x, y) -> Just (x, y, mySign) -- Win if possible
        _ ->
            case finishingMove board oppSign of
                Just (x, y) -> Just (x, y, mySign) -- Block if needed
                _ ->
                    case takeCenter board of
                        Just (x, y) -> Just (x, y, mySign) -- Center if free
                        _ ->
                            case takeAnyEmptyCorner board of
                                Just (x, y) -> Just (x, y, mySign) -- Any corner if free
                                _ ->
                                    case takeAnyEmptyEdge board of
                                        Just (x, y) -> Just (x, y, mySign) -- Any edge if free
                                        _ -> Nothing

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
