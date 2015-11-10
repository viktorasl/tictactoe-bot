module Tictactoe.Def.Move
where
import Data.Maybe
import Tictactoe.Base
import Tictactoe.Move.Base

data ExpectedMove a = NoExp | ExpCenter | ExpAnyCorner | ExpOppositeCorner a | ExpOppositeSelfCorner a
    deriving (Show, Eq)

type ScenarioMove = (BoardField, ExpectedMove Coords)

def :: [ExpectedMove Coords] -> Board -> Maybe (BoardField, (ExpectedMove Coords))
def scens board =
    case matchingScenario scens board of
        Just (moveField, exp) -> Just (moveField, exp) -- By scenario
        _ ->
            case finishingWin board mySign of
                Just (x, y) -> Just ((x, y, mySign), NoExp) -- Win if possible
                _ ->
                    case finishingBlock board oppSign of
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
            case takenAnyCorner board of
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
