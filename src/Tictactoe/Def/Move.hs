module Tictactoe.Def.Move
where
import Data.Maybe
import Tictactoe.Base
import Tictactoe.Move.Base

data ExpectedMove a = NoExp | First | ExpOppositeCorner a | ExpOppositeSelfCorner a
    deriving (Show, Eq)

type ScenarioMove = (BoardField, ExpectedMove Coords)

def :: ExpectedMove Coords -> Board -> Maybe (BoardField, (ExpectedMove Coords))
def exp board =
    case moveByScenario exp board of
        Just scenMove -> Just scenMove -- By scenario
        _ ->
            case defaultMove board mySign oppSign of -- default move
                Just boardField -> Just (boardField, NoExp)
                _ -> Nothing

-- Finishing moves

moveByScenario :: ExpectedMove Coords -> Board -> Maybe ScenarioMove
moveByScenario scen board =
    case scen of
        First ->
            case fieldExists board (1, 1) of
                Just _ -> Just ((0, 0, mySign), ExpOppositeCorner (2, 2)) -- Take any corner
                _ ->
                    case takenAnyCorner board of
                    Just takenCorner' ->
                        case takeCenter board of
                            Just (x, y) -> Just ((x, y, mySign), ExpOppositeSelfCorner (oppositeCorner takenCorner')) -- Take center
                            _ -> Nothing
                    _ -> Nothing
        ExpOppositeCorner coords ->
            case fieldExists board coords of
                Just _ ->
                    case takeAnyEmptyCorner board of
                        Just (x, y) -> Just ((x, y, mySign), NoExp) -- Take any corner
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
