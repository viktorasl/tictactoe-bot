module Main
where
import Data.Char
import Data.List
import Data.Maybe
import Tictactoe.Encoder
import Tictactoe.Base
import Tictactoe.Move

defaultField = (0, 0, mySign)

main :: IO ()
main = do
    putStrLn $ show $ miniPlay [(2, 0), (0, 2), (2, 1)] [ExpCenter, ExpAnyCorner] []

testCase :: (Eq a) => a -> a -> String
testCase res exp = if (res == exp) then "Pass" else "Fail"

testScenarioMoves :: [String]
testScenarioMoves = [
    testCase (moveByScenario ExpCenter [(1, 1, oppSign)]) (Just ((0, 0, mySign), ExpOppositeCorner (2, 2))),
    testCase (moveByScenario ExpAnyCorner [(0, 2, oppSign)]) (Just ((1, 1, mySign), ExpOppositeSelfCorner (2, 0))),
    testCase (moveByScenario (ExpOppositeCorner (0, 2)) [(0, 0, oppSign), (1, 1, oppSign), (0, 2, mySign)]) (Just ((2, 0, mySign), NoExp)),
    testCase (moveByScenario (ExpOppositeSelfCorner (2, 2)) [(0, 0, oppSign), (1, 1, mySign), (2, 2, oppSign)]) (Just ((0, 1, mySign), NoExp))
    ]

testMatchingScenarios :: [String]
testMatchingScenarios = [
    testCase (matchingScenario [ExpCenter, ExpAnyCorner] [(1, 1, oppSign)]) (Just ((0, 0, mySign), ExpOppositeCorner (2, 2))),
    testCase (matchingScenario [ExpCenter, ExpAnyCorner] [(1, 0, oppSign)]) (Nothing),
    testCase (matchingScenario [ExpOppositeSelfCorner (2, 0)] [(0, 2, oppSign), (1, 1, mySign), (2, 0, oppSign)]) (Just ((0, 1, mySign), NoExp))
    ]

testMiniPlays :: [String]
testMiniPlays = [
    testCase (miniPlay [(2, 0), (0, 2), (2, 1)] [ExpCenter, ExpAnyCorner] []) ([(0,1,'o'),(0,2,'x'),(1,1,'o'),(2,0,'x')]),
    testCase (miniPlay [(1, 1), (2, 2), (1, 0)] [ExpCenter, ExpAnyCorner] []) ([(0,2,'o'),(2,2,'x'),(0,0,'o'),(1,1,'x')])
    ]

testFinishingMoves :: [String]
testFinishingMoves = [
    testCase (finishingBlock [(1, 1, 'x'), (2, 2, 'x')]) (Just (0,0)),
    testCase (finishingWin [(1, 1, 'x'), (2, 2, 'x')]) Nothing,
    testCase (finishingBlock [(1,1,'x'),(2,2,'o'),(0,0,'o')]) Nothing,
    testCase (finishingBlock [(1,0,'x'),(1,1,'x')]) (Just (1,2)),
    testCase (finishingWin [(1,0,'x'),(1,1,'x')]) Nothing,
    testCase (finishingWin [(0,0,'x'),(1,1,'x'),(2,2,'o'),(2,0,'x'),(1,0,'o'),(1,2,'o')]) (Just (0,2)),
    testCase (finishingWin [(0,0,'x'),(1,1,'x'),(2,2,'o'),(2,0,'x'),(1,0,'o'),(1,2,'o'),(0,2,'x')]) Nothing,
    testCase (finishingBlock [(0,0,'x'),(1,1,'x'),(2,2,'o'),(2,0,'x'),(1,0,'o'),(1,2,'o'),(0,2,'x')]) (Just (0,1))
    ]

testBoardStringify :: [String]
testBoardStringify = [
    testCase (stringifyBoard [(2,1,'o'),(0,1,'x'),(1,0,'o'),(2,0,'x')]) "ld1:v1:o1:xi2e1:yi1eed1:v1:x1:xi0e1:yi1eed1:v1:o1:xi1e1:yi0eed1:v1:x1:xi2e1:yi0eee"
    ]

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

-- Board parsing

parseBoard :: String -> Board
parseBoard ('l' : list) = parseBoardFields' list []

parseBoardFields' :: String -> Board -> Board
parseBoardFields' ('e' : dict) board = board
parseBoardFields' ('d' : dict) board =
    let
    (field, rest) = parseBoardField' dict defaultField
    in parseBoardFields' rest (field : board)

parseChar :: String -> Char
parseChar str = head $ drop 2 str

parseInt :: String -> Int
parseInt str = digitToInt $ head $ drop 1 str

assignValueToKey :: String -> Char -> BoardField -> (BoardField, String)
assignValueToKey str 'v' (x, y, v) = ((x, y, parseChar str), drop 3 str)
assignValueToKey str 'x' (x, y, v) = ((parseInt str, y, v), drop 3 str)
assignValueToKey str 'y' (x, y, v) = ((x, parseInt str, v), drop 3 str)

parseBoardField' :: String -> BoardField -> (BoardField, String)
parseBoardField' ('e' : rest) field = (field, rest) 
parseBoardField' str field =
    let
    (key, rest) = parseBoardFieldKey str
    (field', rest') = assignValueToKey rest key field
    in parseBoardField' rest' field'

parseBoardFieldKey :: String -> (Char, String)
parseBoardFieldKey str = (head $ drop 2 str, drop 3 str)
