{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldr, init, last, isPrefixOf)
import Data.Text (splitOn, unpack, lines, Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Node = Node { name::Text, content::NodeType }
data NodeType = File Integer | Dir [Node]

main = do
    contents <- TIO.readFile "day7.input"
    let commandLines = filter (not . T.null) (T.lines contents)

    -- skip the first line which is always $ cd /
    let root = parseNode commandLines [] Node{name="", content=Dir []}

    print (walkTree addNode 0 root)
        where addNode Node{content=File size} sum = sum + size
              addNode Node{content=Dir _} sum = sum

type Stack = [Text]

parseNode :: [Text] -> Stack -> Node -> Node
parseNode (line:rest) stack root = parseNode rest newStack newRoot
    where (newStack, newRoot) = operate command stack newRoot
          command = parseLine line
parseNode [] stack root = root

data Command = Cd Text | Noop | FileOutput Integer Text | DirOutput Text
parseLine :: Text -> Command
parseLine line
    | "$ ls " `T.isPrefixOf` line = Noop
    | "$ cd /" `T.isPrefixOf` line = Noop -- this is just the first line anyway
    | "$ cd " `T.isPrefixOf` line = Cd (T.drop 5 line)
    | "dir " `T.isPrefixOf` line = DirOutput (T.drop 4 line)
    | otherwise = FileOutput ((read . unpack) size) name
        where [size, name] = splitOn " " line

operate :: Command -> Stack -> Node -> (Stack, Node)
operate (Cd "..") (stackTop:stack) root = (stack, root)
operate (Cd name) stack root = (name:stack, root)
operate Noop stack root = (stack, root)
operate (DirOutput dirName) stack root = (stack, setNode root (dirName:stack) (Dir []))
operate (FileOutput size fileName) stack root = (stack, setNode root (fileName:stack) (File size))

setNode :: Node -> Stack -> NodeType -> Node
setNode Node{name, content=Dir children} [leafName] leafContent = Node{name, content=Dir (leaf:children)}
    where leaf = Node{name=leafName, content=leafContent}
setNode Node{name, content=Dir children} stack leafContent = Node{name, content=Dir (map mapChild children)}
    where mapChild child@Node{name=childName, content=childContent} =
            if childName == last stack
            then setNode child (init stack) leafContent
            else child

walkTree :: (Node -> t -> t) -> t -> Node -> t
walkTree fn state node@Node{name, content} = do
    let newState = fn node state
    case content of File _ -> newState
                    Dir children -> foldr (\child tempState -> walkTree fn tempState child) newState children
