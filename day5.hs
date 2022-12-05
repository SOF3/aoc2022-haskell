{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Foldable (toList)
import qualified Data.Array as A

type Column = [Char]
type Row = [Char]

main :: IO ()
main = do
    fullFile <- TIO.readFile "day5.input"
    let [header, body] = T.splitOn "\n\n" fullFile

    let _:crateLines = (reverse . T.lines) header
    let crateRows = map parseCrateLine crateLines
    let columns = [crateColumn crateRows column | column <- [0..8]]

    let moves = (filter (\line -> line /= "") . T.lines) body

    putStr "Part 1: "
    (print . map head . runMoves reverse moves) columns

    putStr "Part 2: "
    (print . map head . runMoves id moves) columns

parseCrateLine :: T.Text -> Row
parseCrateLine line = map (T.index line) [(4*i+1) | i <- [0..8]]

crateColumn :: [Row] -> Int -> Column
crateColumn rows columnId = addCrateColumnTo rows columnId []
addCrateColumnTo :: [Row] -> Int -> Column -> Column
addCrateColumnTo [] _ column = column
addCrateColumnTo (bottom:rows) columnId column = case (bottom !! columnId) of
    ' ' -> column
    c -> addCrateColumnTo rows columnId (c:column)

runMoves :: (Column -> Column) -> [T.Text] -> [Column] -> [Column]
runMoves moveOrder [] columns = columns
runMoves moveOrder (firstLine:lines) columns = runMoves moveOrder lines afterMove
    where (count, from, to) = parseMoveLine firstLine
          afterMove = runMove moveOrder count from to columns

runMove :: (Column -> Column) -> Int -> Int -> Int -> [Column] -> [Column]
runMove moveOrder count from to columns = addColumn (moveOrder taken) to droppedColumns
    where (droppedColumns, taken) = takeColumn from count columns

takeColumn :: Int -> Int -> [Column] -> ([Column], Column)
takeColumn columnId count columns = (newColumns, taken)
    where taken = take count (columns!!columnId)
          newColumns = [(if i == columnId then drop count else id) (columns!!i) | i <- [0..8]]
addColumn :: Column -> Int -> [Column] -> [Column]
addColumn taken columnId columns = [(if i == columnId then (++) taken else id) (columns!!i)| i <- [0..8]]

parseMoveLine :: T.Text -> (Int, Int, Int)
parseMoveLine line = (count, from - 1, to - 1)
    where [_, countText, _, fromText, _, toText] = T.splitOn " " line
          count = (read . T.unpack) countText
          from = (read . T.unpack) fromText :: Int
          to = (read . T.unpack) toText :: Int
