module Main where

import Test.QuickCheck
import Tictactoe.Base
import Tictactoe.Move
import Tictactoe.Bencode.Encoder
import Tictactoe.Bencode.Decoder

testCase :: (Eq a) => a -> a -> Bool
testCase res exp = res == exp

miniPlay :: [Coords] -> [ExpectedMove Coords] -> Board -> Board
miniPlay att scen board =
    case att of
        [] -> board
        ((x, y) : left) -> let
            newBoard = ((x, y, oppSign) : board)
            defMove = def scen newBoard
            in case defMove of
                Just (moveField, exp) -> miniPlay left [exp] (moveField : newBoard)
                _ -> board

main :: IO ()
main = do
    quickCheck $ testCase (moveByScenario ExpCenter [(1, 1, 'x')]) (Just ((0, 0, 'o'), ExpOppositeCorner (2, 2)))
    quickCheck $ testCase (moveByScenario ExpAnyCorner [(0, 2, 'x')]) (Just ((1, 1, 'o'), ExpOppositeSelfCorner (2, 0)))
    quickCheck $ testCase (moveByScenario (ExpOppositeCorner (0, 2)) [(0, 0, 'x'), (1, 1, 'x'), (0, 2, 'o')]) (Just ((2, 0, 'o'), NoExp))
    quickCheck $ testCase (moveByScenario (ExpOppositeSelfCorner (2, 2)) [(0, 0, 'x'), (1, 1, 'o'), (2, 2, 'o')]) (Just ((0, 1, 'o'), NoExp))
    quickCheck $ testCase (matchingScenario [ExpCenter, ExpAnyCorner] [(1, 1, 'x')]) (Just ((0, 0, 'o'), ExpOppositeCorner (2, 2)))
    quickCheck $ testCase (matchingScenario [ExpCenter, ExpAnyCorner] [(1, 0, 'x')]) (Nothing)
    quickCheck $ testCase (matchingScenario [ExpOppositeSelfCorner (2, 0)] [(0, 2, 'x'), (1, 1, 'o'), (2, 0, 'x')]) (Just ((0, 1, 'o'), NoExp))
    quickCheck $ testCase (miniPlay [(2, 0), (0, 2), (2, 1)] [ExpCenter, ExpAnyCorner] []) ([(0,1,'o'),(0,2,'x'),(1,1,'o'),(2,0,'x')])
    quickCheck $ testCase (miniPlay [(1, 1), (2, 2), (1, 0)] [ExpCenter, ExpAnyCorner] []) ([(0,2,'o'),(2,2,'x'),(0,0,'o'),(1,1,'x')])
    quickCheck $ testCase (finishingBlock [(1, 1, 'x'), (2, 2, 'x')]) (Just (0,0))
    quickCheck $ testCase (finishingWin [(1, 1, 'x'), (2, 2, 'x')]) Nothing
    quickCheck $ testCase (finishingBlock [(1,1,'x'),(2,2,'o'),(0,0,'o')]) Nothing
    quickCheck $ testCase (finishingBlock [(1,0,'x'),(1,1,'x')]) (Just (1,2))
    quickCheck $ testCase (finishingWin [(1,0,'x'),(1,1,'x')]) Nothing
    quickCheck $ testCase (finishingWin [(0,0,'x'),(1,1,'x'),(2,2,'o'),(2,0,'x'),(1,0,'o'),(1,2,'o')]) (Just (0,2))
    quickCheck $ testCase (finishingWin [(0,0,'x'),(1,1,'x'),(2,2,'o'),(2,0,'x'),(1,0,'o'),(1,2,'o'),(0,2,'x')]) Nothing
    quickCheck $ testCase (finishingBlock [(0,0,'x'),(1,1,'x'),(2,2,'o'),(2,0,'x'),(1,0,'o'),(1,2,'o'),(0,2,'x')]) (Just (0,1))
    quickCheck $ testCase (stringifyBoard [(2,1,'o'),(0,1,'x'),(1,0,'o'),(2,0,'x')]) "ld1:v1:o1:xi2e1:yi1eed1:v1:x1:xi0e1:yi1eed1:v1:o1:xi1e1:yi0eed1:v1:x1:xi2e1:yi0eee"
