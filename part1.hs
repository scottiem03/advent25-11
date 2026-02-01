import Data.List

-- utility functions

split :: Char -> String -> [String]
split _ "" = []
split c s = [thisPart] ++ split c restOfString where
  (thisPart,tooLong) = break (==c) s
  restOfString = case tooLong of
    [] -> ""
    _ -> tail tooLong

prettyList :: (Show a) => [a] -> String
prettyList [] = ""
prettyList (x:xs) = (show x) ++ "\n" ++ prettyList xs

pp :: (Show a) => [a] -> IO ()
pp xs = putStrLn $ prettyList xs

-- AoC 2025 - Day 11 - Part One - solution code

data Node = Node String [String]
  deriving (Eq,Show)

type Path = String

processLine :: String -> Node
processLine s = newNode where
  myTokens = words s
  myName = (init . head) myTokens
  myOutputs = tail myTokens
  newNode = Node myName myOutputs

justNames :: [Node] -> [String]
justNames [] = []
justNames ((Node name outs):ns) = [name] ++ justNames ns

justOuts :: [Node] -> [String]
justOuts [] = []
justOuts ((Node name outs):ns) = union outs (justOuts ns)

originNode :: [Node] -> String
originNode ns = head $ (justNames ns) \\ (justOuts ns)

-- fortunately, both the example data set and the big data set
-- have one origin node and only one dead-end node ("out")

nodeToPaths :: Node -> [String]
nodeToPaths (Node name kids) = map (\x -> name ++ " " ++ x) kids

expandNode :: String -> [Node] -> [String]
expandNode _ [] = []
expandNode target ((Node name kids):ns)
  | target == name = nodeToPaths (Node name kids)
  | otherwise = expandNode target ns

isComplete :: Path -> Bool
isComplete thisPath = last (words thisPath) == "out"

allButLast :: Path -> Path
allButLast p = foldl1 (\x y -> x ++ " " ++ y) (init $ words p)

growPath :: Path -> [Node] -> [Path]
growPath p myNodes
  | isComplete p = [p]
  | otherwise = map (\x -> allButLast p ++ " " ++ x) (expandNode (last $ words p) myNodes) 

recursePaths :: [Path] -> [Node] -> [Path]
recursePaths ps myNodes
  | all isComplete ps = ps
  | otherwise = recursePaths (concatMap (\x -> growPath x myNodes) ps) myNodes

main = do
  myFile <- readFile "data11.txt"
  let myData = map processLine $ lines myFile
  
  print $ length $ recursePaths (expandNode "you" myData) myData
