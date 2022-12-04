{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Foldable (toList)
import Data.List (sortOn)

main :: IO ()
main = do
    foods <- readFoods
    putStr "Part 1: "
    let sorted = sortOn negate (toList foods)
    let a : b : c : remain = sorted
    print a
    putStr "Part 2: "
    print (a + b + c)

readFoods :: IO [Integer]
readFoods = do
    contents <- TIO.readFile "day1.input"
    let lines = T.splitOn "\n\n" contents
    return (map sumRows lines)

sumRows :: T.Text -> Integer
sumRows buf = sum ints
    where
        lines = filter (\t -> t /= "") (T.splitOn ("\n" :: T.Text) buf)
        ints = map (read . T.unpack) lines :: [Integer]
