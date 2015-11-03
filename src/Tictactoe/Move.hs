module Tictactoe.Move
where
import Data.Maybe
import Tictactoe.Base

data ExpectedMove a = NoExp | ExpCenter | ExpAnyCorner | ExpOppositeCorner a | ExpOppositeSelfCorner a
    deriving (Show, Eq)
data RowInfo = RowInfo {
    free :: [Coords]
    , matchSign :: Int
} deriving Show

type ScenarioMove = (BoardField, ExpectedMove Coords)

def :: [ExpectedMove Coords] -> Board -> Maybe (BoardField, (ExpectedMove Coords))
def scens board =
    case matchingScenario scens board of
        Just (moveField, exp) -> Just (moveField, exp) -- By scenario
        _ ->
            case finishingWin board of
                Just (x, y) -> Just ((x, y, mySign), NoExp) -- Win if possible
                _ ->
                    case finishingBlock board of
                        Just (x, y) -> Just ((x, y, mySign), NoExp) -- Block if needed
                        _ ->
                            case takeCenter board of
                                Just (x, y) -> Just ((x, y, mySign), NoExp) -- Center if free
                                _ ->
                                    case takeAnyEmptyCorner board of
                                        Just (x, y) -> Just ((x, y, mySign), NoExp) -- Any corner if free
                                        _ ->
                                            case takeAnyEmptyEdge board of
                                                Just (x, y) -> Just ((x, y, mySign), NoExp) -- Any edge if free
                                                _ -> Nothing

-- Finishing moves

finishingWin :: Board -> Maybe Coords
finishingWin board = finishingMove board mySign

finishingBlock :: Board -> Maybe Coords
finishingBlock block = finishingMove block oppSign

finishingMove :: Board -> Char -> Maybe Coords
finishingMove board sign = let
    infos = (finishHorizontal board sign) ++ (finishVertical board sign) ++ (finishDiagonal board sign)
    satInfos = filter (\info -> ((matchSign info) == 2) && ((length (free info)) == 1)) infos
    in listToMaybe $ map (\info -> head (free info)) satInfos

finishHorizontal :: Board -> Char -> [RowInfo]
finishHorizontal board sign = map (\seqCoords -> seqInfo seqCoords board sign) [[(a,b) | b <- [0..2]] | a <- [0..2]]

finishVertical :: Board -> Char -> [RowInfo]
finishVertical board sign = map (\seqCoords -> seqInfo seqCoords board sign) [[(a,b) | a <- [0..2]] | b <- [0..2]]

diagonals :: [[Coords]]
diagonals = [
    [(a,a) | a <- [0..2]],
    [(a,abs (a - 2)) | a <- [0..2]]
    ]

finishDiagonal :: Board -> Char -> [RowInfo]
finishDiagonal board sign = map (\seqCoords -> seqInfo seqCoords board sign) diagonals

seqInfo :: [Coords] -> Board -> Char -> RowInfo
seqInfo coords board sign = seqInfo' coords board sign (RowInfo [] 0)

seqInfo' :: [Coords] -> Board -> Char -> RowInfo -> RowInfo
seqInfo' coords board sign rowInfo =
    case coords of
        [] -> rowInfo
        (c : left) ->
            case fieldExists board c of
                Just (x, y, s) ->
                    if s == sign then
                        seqInfo' left board sign (RowInfo (free rowInfo) ((matchSign rowInfo) + 1))
                    else
                        seqInfo' left board sign (RowInfo (free rowInfo) (matchSign rowInfo))
                Nothing -> seqInfo' left board sign (RowInfo (c : (free rowInfo)) (matchSign rowInfo))

matchingScenario :: [ExpectedMove Coords] -> Board -> Maybe ScenarioMove
matchingScenario scens board = 
    listToMaybe $
        map fromJust $
            filter isJust $
                map (\scen -> moveByScenario scen board) scens

moveByScenario :: ExpectedMove Coords -> Board -> Maybe ScenarioMove
moveByScenario scen board =
    case scen of
        ExpCenter ->
            case fieldExists board (1, 1) of
                Just _ -> Just ((0, 0, mySign), ExpOppositeCorner (2, 2)) -- Take any corner
                _ -> Nothing
        ExpOppositeCorner coords ->
            case fieldExists board coords of
                Just _ ->
                    case takeAnyEmptyCorner board of
                        Just (x, y) -> Just ((x, y, mySign), NoExp) -- Take any corner
                        _ -> Nothing
                _ -> Nothing
        ExpAnyCorner ->
            case takenCorner board of
                Just takenCorner' ->
                    case takeCenter board of
                        Just (x, y) -> Just ((x, y, mySign), ExpOppositeSelfCorner (oppositeCorner takenCorner')) -- Take center
                        _ -> Nothing
                _ -> Nothing
        ExpOppositeSelfCorner coords ->
            case fieldExists board coords of
                Just _ ->
                    case takeAnyEmptyEdge board of
                        Just (x, y) -> Just ((x, y, mySign), NoExp) -- Take any empty edge
                        _ -> Nothing
                _ -> Nothing
        NoExp -> Nothing

takenCorner :: Board -> Maybe Coords
takenCorner board = listToMaybe $ filter (\coords' -> isJust (fieldExists board coords')) [(0, 0), (0, 2), (2, 0), (2, 2)]

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
