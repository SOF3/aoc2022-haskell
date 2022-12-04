import Data.Char (ord)
import Data.List (sort)

main :: IO ()
main = do
    contents <- readFile "day3.input"
    let racksacks = lines contents
    putStr "Part 1: "
    (print . sum . map (priority . getDuplicate)) racksacks
    putStr "Part 2: "
    (print . sum . map priority . map3 findCommon3) (map sort racksacks)

getDuplicate :: String -> Char
getDuplicate text = do
    let separator = quot (length text) 2
    let (front, back) = splitAt separator text
    findCommon (sort front) (sort back)

findCommon :: String -> String -> Char
findCommon a b
    | (head a) == (head b) = head a
    | (head a) < (head b) = findCommon (tail a) b
    | (head a) > (head b) = findCommon a (tail b)

priority :: Char -> Int
priority char
    | ord('A') <= ord(char) && ord(char) <= ord('Z') = ord(char) - ord('A') + 27
    | ord('a') <= ord(char) && ord(char) <= ord('z') = ord(char) - ord('a') + 1

map3 :: forall s t. (s -> s -> s -> t) -> [s] -> [t]
map3 f (s1:s2:s3:srest) = (f s1 s2 s3) : (map3 f srest)
map3 f [] = []

findCommon3 :: String -> String -> String -> Char
findCommon3 (a0:a) (b0:b) (c0:c)
    | a0 == b0 && b0 == c0 = a0
    | a0 <= b0 && a0 <= c0 = findCommon3 a (b0:b) (c0:c)
    | b0 <= a0 && b0 <= c0 = findCommon3 (a0:a) b (c0:c)
    | c0 <= a0 && c0 <= b0 = findCommon3 (a0:a) (b0:b) c
