{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Foldable (toList)

main :: IO ()
main = do
    lines <- TIO.readFile "day2.input"
    putStr "Part 1: "
    (print . sum . map getScore1 . (filter (\line -> line /= ""))) (T.splitOn "\n" lines)
    putStr "Part 2: "
    (print . sum . map getScore2 . (filter (\line -> line /= ""))) (T.splitOn "\n" lines)

getScore1 :: T.Text -> Int
getScore1 "A X" = 3 + 1
getScore1 "A Y" = 6 + 2
getScore1 "A Z" = 0 + 3
getScore1 "B X" = 0 + 1
getScore1 "B Y" = 3 + 2
getScore1 "B Z" = 6 + 3
getScore1 "C X" = 6 + 1
getScore1 "C Y" = 0 + 2
getScore1 "C Z" = 3 + 3

getScore2 :: T.Text -> Int
getScore2 "A X" = 0 + 3
getScore2 "A Y" = 3 + 1
getScore2 "A Z" = 6 + 2
getScore2 "B X" = 0 + 1
getScore2 "B Y" = 3 + 2
getScore2 "B Z" = 6 + 3
getScore2 "C X" = 0 + 2
getScore2 "C Y" = 3 + 3
getScore2 "C Z" = 6 + 1
