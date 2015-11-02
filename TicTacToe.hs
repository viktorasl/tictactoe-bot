module TicTacToe
where
import Data.Char
import Data.List
import Data.Maybe

type Coords = (Int, Int)
type BoardField = (Int, Int, Char)
type Board = [BoardField]

defaultField = (0, 0, 'o')
mySign = 'o'
oppSign = 'x'

data ExpectedMove = ExpCenter | ExpAnyCorner | ExpOppositeCorner | ExpOppositeSelfCorner
    deriving Show
data List a = Empty | Cell a [(List a)]
    deriving Show

expectedCenter :: List ExpectedMove
expectedCenter = Cell ExpCenter [Cell ExpOppositeCorner []]

expectedAnyCorner :: List ExpectedMove
expectedAnyCorner = Cell ExpAnyCorner [Cell ExpOppositeSelfCorner []]

testOne :: Maybe (BoardField, [List ExpectedMove])
testOne = moveByScenario Nothing Nothing expectedCenter [(1, 1, oppSign)]

testTwo :: Maybe (BoardField, [List ExpectedMove])
testTwo = moveByScenario Nothing Nothing expectedAnyCorner [(0, 0, oppSign)]

testThree :: Maybe (BoardField, [List ExpectedMove])
testThree = moveByScenario (Just (2, 2, mySign)) (Just (0, 0, oppSign)) (Cell ExpOppositeCorner []) [(0, 0, oppSign), (1, 1, oppSign), (2, 2, mySign)]

testFour :: Maybe (BoardField, [List ExpectedMove])
testFour = moveByScenario (Just (1, 1, mySign)) (Just (0, 2, oppSign)) (Cell ExpOppositeSelfCorner []) [(0, 2, oppSign), (1, 1, mySign), (2, 0, oppSign)]

expectedScenarios :: [List ExpectedMove]
expectedScenarios = [expectedCenter, expectedAnyCorner]

matchingScenario :: Maybe BoardField -> Maybe BoardField -> [List ExpectedMove] -> Board -> Maybe (BoardField, [List ExpectedMove])
matchingScenario myPrev oppPrev scens board =
    listToMaybe $
        map fromJust $
            filter isJust $
                map (\scen -> moveByScenario myPrev oppPrev scen board) scens

testMachingScenario1 :: Maybe (BoardField, [List ExpectedMove])
testMachingScenario1 = matchingScenario Nothing Nothing expectedScenarios [(0, 2, oppSign)]

testMachingScenario2 :: Maybe (BoardField, [List ExpectedMove])
testMachingScenario2 = matchingScenario Nothing Nothing expectedScenarios [(1, 1, oppSign)]

moveByScenario :: Maybe BoardField -> Maybe BoardField -> List ExpectedMove -> Board -> Maybe (BoardField, [List ExpectedMove])
moveByScenario myPrev oppPrev scen board =
    case (myPrev, oppPrev, scen, board) of
        (_, _, Cell ExpCenter nextScen, board) ->
            case (indexOfField board (1, 1)) of
                Just _ -> Just ((0, 0, mySign), nextScen) -- Take any corner
                _ -> Nothing
        (_, _, Cell ExpAnyCorner nextScen, board) ->
            case (takenCorner board) of
                Just _ -> 
                    case takeCenter board of
                        Just (x, y) -> Just ((x, y, mySign), nextScen) -- Take center
                        _ -> Nothing
                _ -> Nothing
        (Just (x, y, _), _, Cell ExpOppositeCorner nextScen, board) ->
            case (indexOfField board (oppositeCorner (x, y))) of
                Just _ ->
                    case takeAnyEmptyCorner board of
                        Just (anyX, anyY) -> Just ((anyX, anyY, mySign), nextScen) -- Take any empty corner
                        _ -> Nothing
                _ -> Nothing
        (_, Just (x, y, _), Cell ExpOppositeSelfCorner nextScen, board) ->
            case (indexOfField board (oppositeCorner (x, y))) of
                Just _ ->
                    case takeAnyDiagonalLine board of
                        Just (anyX, anyY) -> Just ((anyX, anyY, mySign), nextScen) -- Take any empty diagonal line
                        _ -> Nothing
                _ -> Nothing
        _ -> Nothing

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

