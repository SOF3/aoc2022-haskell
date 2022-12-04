{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    contents <- TIO.readFile "day4.input"
    let pairs = (map parsePair . T.lines) contents
    putStr "Part 1: "
    (print . length . filter isCompleteOverlap) pairs
    putStr "Part 2: "
    (print . length . filter isPartialOverlap) pairs

type Pair = (Range, Range)
type Range = (Int, Int)

parsePair :: T.Text -> Pair
parsePair line = do
    let [a, b] = T.splitOn "," line
    (parseRange a, parseRange b)

parseRange :: T.Text -> Range
parseRange s = do
    let [start, end] = T.splitOn "-" s
    ((read . T.unpack) start, (read . T.unpack) end)

isCompleteOverlap :: Pair -> Bool
isCompleteOverlap (a, b) = isSuperset a b || isSuperset b a
isSuperset a b = fst a <= fst b && snd a >= snd b

isPartialOverlap :: Pair -> Bool
isPartialOverlap (a, b) = fst c <= snd c
    where c = intersect a b
intersect a b = (max (fst a) (fst b), min (snd a) (snd b))
