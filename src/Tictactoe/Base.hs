module Tictactoe.Base (
    Coords,
    BoardField,
    Board,
    BoardState(Won,Tie,Ongoing),
    RowInfo(RowInfo),
    matchSign,
    free,
    fieldExists,
    coordsEqual,
    mySign,
    oppSign,
    horizontalInfos,
    verticalInfos,
    diagonalInfos
) where
import Data.Maybe

data RowInfo = RowInfo {
    free :: [Coords]
    , matchSign :: Int
} deriving Show

data BoardState = Won | Tie | Ongoing
type Coords = (Int, Int)
type BoardField = (Int, Int, Char)
type Board = [BoardField]

mySign = 'o'
oppSign = 'x'

fieldExists :: Board -> Coords -> Maybe BoardField
fieldExists board coords = listToMaybe $ filter (\field' -> coordsEqual coords field') board

coordsEqual :: Coords -> BoardField -> Bool
coordsEqual (x1, y1) (x2, y2, _) = if (x1 == x2 && y1 == y2) then True else False 

horizontalInfos :: Board -> Char -> [RowInfo]
horizontalInfos board sign = map (\seqCoords -> seqInfo seqCoords board sign) [[(a,b) | b <- [0..2]] | a <- [0..2]]

verticalInfos :: Board -> Char -> [RowInfo]
verticalInfos board sign = map (\seqCoords -> seqInfo seqCoords board sign) [[(a,b) | a <- [0..2]] | b <- [0..2]]

diagonals :: [[Coords]]
diagonals = [
    [(a,a) | a <- [0..2]],
    [(a,abs (a - 2)) | a <- [0..2]]
    ]

diagonalInfos :: Board -> Char -> [RowInfo]
diagonalInfos board sign = map (\seqCoords -> seqInfo seqCoords board sign) diagonals

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
