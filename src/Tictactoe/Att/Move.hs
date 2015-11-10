module Tictactoe.Att.Move
where
import Data.Maybe
import Tictactoe.Base
import Tictactoe.Move.Base

data ExpectedMove a b = NoExp | First | Either a b
    deriving (Show, Eq)

type ScenarioMove = (BoardField, ExpectedMove Coords Coords)

moveByScenario :: ExpectedMove Coords Coords -> Board -> Maybe ScenarioMove
moveByScenario scen board =
    case scen of
        First ->
            case takenAnyEdge board of
                Just takenCoords ->
                    case takenCoords of
                        (x, 1) -> Just ((abs (x-2), 0, oppSign), NoExp)
                        (1, y) -> Just ((0, abs (y-2), oppSign), NoExp)
                _ ->
                    case takenAnyCorner board of
                        Just takenCoords -> let
                            (x, y) = oppositeCorner takenCoords
                            in Just ((x, y, oppSign), Either (x, 1) (1, y))
                        _ -> Nothing
        Either (x1, y1) (x2, y2) ->
            case fieldExists board (x1, y1) of
                Just _ -> Just ((abs (x1-2), y2, oppSign), NoExp)
                _ ->
                    case fieldExists board (x2, y2) of
                        Just _ -> Just ((x1, abs (y2-2), oppSign), NoExp)
                        _ -> Nothing
        NoExp -> Nothing
