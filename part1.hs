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

data StrNode = StrNode String [String]
  deriving (Eq,Show)

data Path = Path [String]
  deriving (Eq)

instance Show Path where
  show (Path ps) = foldl1 (\x y -> x ++ " " ++ y) ps

processLine :: String -> StrNode
processLine s = newNode where
  myTokens = words s
  myName = (init . head) myTokens
  myOutputs = tail myTokens
  newNode = StrNode myName myOutputs

justNames :: [StrNode] -> [String]
justNames [] = []
justNames ((StrNode name outs):ns) = [name] ++ justNames ns

justOuts :: [StrNode] -> [String]
justOuts [] = []
justOuts ((StrNode name outs):ns) = union outs (justOuts ns)

originNode :: [StrNode] -> String
originNode ns = head $ (justNames ns) \\ (justOuts ns)

-- fortunately, both the example data set and the big data set
-- have one origin node and only one dead-end node ("out")

initialPath :: [StrNode] -> [Path]
initialPath nodes = [Path [originNode nodes]]

checkComplete :: Path -> Bool
checkComplete (Path ns) = last ns == "out"

checkPaths :: [Path] -> [StrNode] -> [Path]
checkPaths ps nodes
  | all checkComplete ps = ps
  | otherwise = checkPaths (expandPaths ps nodes) nodes

expandPaths :: [Path] -> [StrNode] -> [Path]
expandPaths [] _ = []
expandPaths (p:ps) nodes
  | checkComplete p = expandPaths ps nodes
  | otherwise = (grow p nodes) ++ expandPaths ps nodes

grow :: Path -> [StrNode] -> [Path]
grow p nodes = newPaths where
  (Path pathStrs) = p
  lastItem = last pathStrs
  nextLayer = getChildren lastItem nodes
  newPaths = map (graft p) nextLayer

graft :: Path -> String -> Path
graft (Path names) name = Path (names ++ [name])

getChildren :: String -> [StrNode] -> [String]
getChildren _ [] = []
getChildren name ((StrNode n kids):ns)
  | name == n = kids
  | otherwise = getChildren name ns

main = do
  myFile <- readFile "exdata11.txt"
  let myData = map processLine $ lines myFile

  pp $ myData
