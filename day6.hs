main :: IO ()
main = do
    stream <- readFile "day6.input"
    putStr "Part 1: "
    print (findMarker stream 0 4)
    putStr "Part 2: "
    print (findMarker stream 0 14)

findMarker :: [Char] -> Int -> Int -> Int
findMarker stream prevLength request
    | length stream >= request && isDistinct (take request stream) = prevLength + request
    | otherwise = findMarker (tail stream) (prevLength + 1) request

isDistinct :: [Char] -> Bool
isDistinct (first:rest) = notElem first rest && isDistinct rest
isDistinct [] = True
