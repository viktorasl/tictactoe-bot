module TicTacToe
where
import Data.Char
import Data.List
import Data.Maybe

mySign = 'o'
oppSign = 'x'
defaultField = (0, 0, mySign)

data ExpectedMove a = NoExp | ExpCenter | ExpAnyCorner | ExpOppositeCorner a | ExpOppositeSelfCorner a
    deriving (Show, Eq)

type Coords = (Int, Int)
type BoardField = (Int, Int, Char)
type Board = [BoardField]
type ScenarioMove = (Maybe BoardField, ExpectedMove Coords)

testCase :: (Eq a) => a -> a -> String
testCase res exp = if (res == exp) then "Pass" else "Fail"

testScenarioMoves :: [String]
testScenarioMoves = [
    testCase (moveByScenario ExpCenter [(1, 1, oppSign)]) (Just (0, 0, mySign), ExpOppositeCorner (2, 2)),
    testCase (moveByScenario ExpAnyCorner [(0, 2, oppSign)]) (Just (1, 1, mySign), ExpOppositeSelfCorner (2, 0)),
    testCase (moveByScenario (ExpOppositeCorner (0, 2)) [(0, 0, oppSign), (1, 1, oppSign), (0, 2, mySign)]) (Just (2, 0, mySign), NoExp),
    testCase (moveByScenario (ExpOppositeSelfCorner (2, 2)) [(0, 0, oppSign), (1, 1, mySign), (2, 2, oppSign)]) (Just (0, 1, mySign), NoExp)
    ]

testMatchingScenarios :: [String]
testMatchingScenarios = [
    testCase (matchingScenario [ExpCenter, ExpAnyCorner] [(1, 1, oppSign)]) (Just (Just (0, 0, mySign), ExpOppositeCorner (2, 2))),
    testCase (matchingScenario [ExpCenter, ExpAnyCorner] [(1, 0, oppSign)]) (Nothing),
    testCase (matchingScenario [ExpOppositeSelfCorner (2, 0)] [(0, 2, oppSign), (1, 1, mySign), (2, 0, oppSign)]) (Just (Just (0, 1, mySign), NoExp))
    ]

matchingScenario :: [ExpectedMove Coords] -> Board -> Maybe ScenarioMove
matchingScenario scens board = 
    listToMaybe $
            filter (\(field, _) -> isJust field) $
                map (\scen -> moveByScenario scen board) scens

moveByScenario :: ExpectedMove Coords -> Board -> ScenarioMove
moveByScenario scen board =
    case scen of
        ExpCenter ->
            case indexOfField board (1, 1) of
                Just _ -> (Just (0, 0, mySign), ExpOppositeCorner (2, 2)) -- Take any corner
                _ -> (Nothing, NoExp)
        ExpOppositeCorner coords ->
            case indexOfField board coords of
                Just _ ->
                    case takeAnyEmptyCorner board of
                        Just (x, y) -> (Just (x, y, mySign), NoExp) -- Take any corner
                        _ -> (Nothing, NoExp)
                _ -> (Nothing, NoExp)
        ExpAnyCorner ->
            case takenCorner board of
                Just takenCorner' ->
                    case takeCenter board of
                        Just (x, y) -> (Just (x, y, mySign), ExpOppositeSelfCorner (oppositeCorner takenCorner')) -- Take center
                        _ -> (Nothing, NoExp)
                _ -> (Nothing, NoExp)
        ExpOppositeSelfCorner coords ->
            case indexOfField board coords of
                Just _ ->
                    case takeAnyDiagonalLine board of
                        Just (x, y) -> (Just (x, y, mySign), NoExp) -- Take any empty diagonal line
                        _ -> (Nothing, NoExp)
                _ -> (Nothing, NoExp)
        NoExp -> (Nothing, NoExp)

takenCorner :: Board -> Maybe Coords
takenCorner board = listToMaybe $ filter (\coords' -> isJust (indexOfField board coords')) [(0, 0), (0, 2), (2, 0), (2, 2)]

oppositeCorner :: Coords -> Coords
oppositeCorner (x, y) = (abs (x - 2), abs (y - 2))

takeCenter :: Board -> Maybe Coords
takeCenter board =
    case (indexOfField board (1, 1)) of
        Just _ -> Nothing
        Nothing -> Just (1, 1)

takeAnyEmptyCorner :: Board -> Maybe Coords
takeAnyEmptyCorner board = listToMaybe $ filter (\coords' -> isNothing (indexOfField board coords')) [(0, 0), (0, 2), (2, 0), (2, 2)]

takeAnyDiagonalLine :: Board -> Maybe Coords
takeAnyDiagonalLine board = listToMaybe $ filter (\coords' -> isNothing (indexOfField board coords')) [(0, 1), (1, 0), (1, 2), (2, 1)]

{-
message to react to
board:
+-+-+-+
| |X| |
+-+-+-+
|O| | |
+-+-+-+
|X|O| |
+-+-+-+
-}
message :: String
message = "ld1:v1:x1:xi2e1:yi0eed1:v1:o1:xi1e1:yi0eed1:v1:x1:xi0e1:yi1eed1:v1:o1:xi2e1:yi1eee"

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

-- Field lookup

indexOfField :: Board -> Coords -> Maybe Int
indexOfField board coords = findIndex (\field' -> coordsEqual coords field') board

coordsEqual :: Coords -> BoardField -> Bool
coordsEqual (x1, y1) (x2, y2, _) = if (x1 == x2 && y1 == y2) then True else False 

