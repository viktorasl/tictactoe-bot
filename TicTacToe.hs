module TicTacToe
where
import Data.Char
import Data.List

type Coords = (Int, Int)
type BoardField = (Int, Int, Char)
type Board = [BoardField]

defaultField = (0, 0, 'o')

data ExpectedMove = ExpCenter | ExpAnyCorner | ExpOppositeCorner | ExpOppositeSelfCorner
    deriving Show
data List a = Empty | Cell a [(List a)]
    deriving Show

mockCenter :: Board
mockCenter = [(1, 1, 'x')]

expectedScenarios :: [List ExpectedMove]
expectedScenarios = [Cell ExpCenter [Cell ExpOppositeCorner []], Cell ExpAnyCorner [Cell ExpOppositeSelfCorner []]]

move :: Maybe BoardField -> Maybe BoardField -> List ExpectedMove -> Board -> (BoardField, [List ExpectedMove])
move myPrev oppPrev scen board =
    case (myPrev, oppPrev, scen, board) of
        (_, _, Cell ExpCenter nextScen, board) -> (defaultField, [Cell ExpCenter nextScen])
        (_, _, Cell ExpAnyCorner nextScen, board) -> (defaultField, [Cell ExpCenter nextScen])
        (myPrev, _, Cell ExpOppositeCorner nextScen, board) -> (defaultField, [Cell ExpCenter nextScen])
        (_, oppPrev, Cell ExpOppositeSelfCorner nextScen, board) -> (defaultField, [Cell ExpCenter nextScen])
        (_, _, Empty, board) -> (defaultField, [])

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

