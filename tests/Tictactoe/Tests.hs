module Main where

import Test.QuickCheck
import Tictactoe.Base
import Tictactoe.Def.Move
import Tictactoe.Bencode.Encoder as Bencode
import Tictactoe.Bencode.Decoder as Bencode
import Tictactoe.BencodeDict.Encoder as BencodeDict
import Tictactoe.BencodeDict.Decoder as BencodeDict

testCase :: (Eq a) => a -> a -> Bool
testCase res exp = res == exp

main :: IO ()
main = do
    quickCheck $ testCase (moveByScenario ExpCenter [(1, 1, 'x')]) (Just ((0, 0, 'o'), ExpOppositeCorner (2, 2)))
    quickCheck $ testCase (moveByScenario ExpAnyCorner [(0, 2, 'x')]) (Just ((1, 1, 'o'), ExpOppositeSelfCorner (2, 0)))
    quickCheck $ testCase (moveByScenario (ExpOppositeCorner (0, 2)) [(0, 0, 'x'), (1, 1, 'x'), (0, 2, 'o')]) (Just ((2, 0, 'o'), NoExp))
    quickCheck $ testCase (moveByScenario (ExpOppositeSelfCorner (2, 2)) [(0, 0, 'x'), (1, 1, 'o'), (2, 2, 'o')]) (Just ((0, 1, 'o'), NoExp))
    quickCheck $ testCase (matchingScenario [ExpCenter, ExpAnyCorner] [(1, 1, 'x')]) (Just ((0, 0, 'o'), ExpOppositeCorner (2, 2)))
    quickCheck $ testCase (matchingScenario [ExpCenter, ExpAnyCorner] [(1, 0, 'x')]) (Nothing)
    quickCheck $ testCase (matchingScenario [ExpOppositeSelfCorner (2, 0)] [(0, 2, 'x'), (1, 1, 'o'), (2, 0, 'x')]) (Just ((0, 1, 'o'), NoExp))
    quickCheck $ testCase (finishingBlock [(1, 1, 'x'), (2, 2, 'x')]) (Just (0,0))
    quickCheck $ testCase (finishingWin [(1, 1, 'x'), (2, 2, 'x')]) Nothing
    quickCheck $ testCase (finishingBlock [(1,1,'x'),(2,2,'o'),(0,0,'o')]) Nothing
    quickCheck $ testCase (finishingBlock [(1,0,'x'),(1,1,'x')]) (Just (1,2))
    quickCheck $ testCase (finishingWin [(1,0,'x'),(1,1,'x')]) Nothing
    quickCheck $ testCase (finishingWin [(0,0,'x'),(1,1,'x'),(2,2,'o'),(2,0,'x'),(1,0,'o'),(1,2,'o')]) (Just (0,2))
    quickCheck $ testCase (finishingWin [(0,0,'x'),(1,1,'x'),(2,2,'o'),(2,0,'x'),(1,0,'o'),(1,2,'o'),(0,2,'x')]) Nothing
    quickCheck $ testCase (finishingBlock [(0,0,'x'),(1,1,'x'),(2,2,'o'),(2,0,'x'),(1,0,'o'),(1,2,'o'),(0,2,'x')]) (Just (0,1))
    quickCheck $ testCase (Bencode.parseBoard "ld1:v1:o1:xi2e1:yi1eed1:v1:x1:xi0e1:yi1eed1:v1:o1:xi1e1:yi0eed1:v1:x1:xi2e1:yi0eee") [(2,0,'x'),(1,0,'o'),(0,1,'x'),(2,1,'o')]
    quickCheck $ testCase (Bencode.stringifyBoard [(2,1,'o'),(0,1,'x'),(1,0,'o'),(2,0,'x')]) "ld1:v1:o1:xi2e1:yi1eed1:v1:x1:xi0e1:yi1eed1:v1:o1:xi1e1:yi0eed1:v1:x1:xi2e1:yi0eee"
    quickCheck $ testCase (BencodeDict.parseBoard "d1:0d1:v1:x1:xi1e1:yi1ee1:1d1:v1:o1:xi1e1:yi0ee1:2d1:v1:x1:xi0e1:yi2ee1:3d1:v1:o1:xi0e1:yi0eee") [(0,0,'o'),(0,2,'x'),(1,0,'o'),(1,1,'x')]
    quickCheck $ testCase (BencodeDict.stringifyBoard [(1,1,'x'),(1,0,'o'),(0,2,'x'),(0,0,'o')]) "d1:0d1:v1:x1:xi1e1:yi1ee1:1d1:v1:o1:xi1e1:yi0ee1:2d1:v1:x1:xi0e1:yi2ee1:3d1:v1:o1:xi0e1:yi0eee"
    quickCheck $ testCase (gameState [] 'x') Ongoing
    quickCheck $ testCase (gameState [(1,1,'x'),(2,2,'x'),(0,0,'o')] 'x') Ongoing
    quickCheck $ testCase (gameState [(1,1,'x'),(2,2,'x'),(0,0,'x')] 'x') Won
    quickCheck $ testCase (gameState [(1,1,'x'),(2,2,'x'),(0,0,'x')] 'o') Lost
    quickCheck $ testCase (gameState [(0,0,'x'),(0,1,'x'),(0,2,'o'),(1,0,'o'),(1,1,'o'),(1,2,'x'),(2,0,'x'),(2,1,'x'),(2,2,'o')] 'x') Tie
    