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

lookupSrt :: (Ord a) => a -> [(a,b)] -> b
lookupSrt target list
  | target == midPoint = midCargo
  | target < midPoint = lookupSrt target firstHalf
  | target > midPoint = lookupSrt target secondHalf
  where
    midIndex = div (length list) 2
    midPoint = fst $ list !! midIndex
    midCargo = snd $ list !! midIndex
    (firstHalf,secondHalf) = splitAt midIndex list
    

-- AoC 2025 - Day 11 - Part One - solution code

data Graph = Node String [Graph] | Out
  deriving (Eq,Show)

processLine :: String -> (String,[String])
processLine s = (name,kids) where
  myTokens = words s
  name = (init . head) myTokens
  kids = tail myTokens

findStrKids :: String -> [(String,[String])] -> [String]
findStrKids target list = lookupSrt target list

buildGraph :: Graph -> [(String,[String])] -> Graph
buildGraph Out _ = Out
buildGraph (Node name kids) myMap = Node name (map (\x -> buildGraph x myMap) newKids) where
  kidNames = findStrKids name myMap
  newKids = map (\x -> case x of "out" -> Out; _ -> Node x []) kidNames

buildGraphTo :: String -> Graph -> [(String,[String])] -> Graph
buildGraphTo _ Out _ = Out
buildGraphTo target (Node name kids) _
  | target == name = (Node name [])
buildGraphTo target (Node name kids) myMap = Node name (map (\x -> buildGraphTo target x myMap) newKids) where
  kidNames = findStrKids name myMap
  newKids = map (\x -> case x of "out" -> Out; _ -> Node x []) kidNames

cleanUp :: String -> Graph -> Graph
cleanUp _ Out = Out
cleanUp target (Node name kids) = Node name (filter (leadsTo target) kids)

leadsTo :: String -> Graph -> Bool
leadsTo _ Out = False
leadsTo target (Node name kids)
  | target == name = True
  | otherwise = foldl1 (||) (map (leadsTo target) kids)

getName :: Graph -> String
getName Out = "out"
getName (Node name kids) = name

findKidByName :: String -> [Graph] -> Graph
findKidByName _ [] = error "kid not found"
findKidByName name (k:ks)
  | getName k == name = k
  | otherwise = findKidByName name ks

-- numPathsBetween :: String -> String -> [(String,[String])]

pathsTo :: String -> Graph -> Int
pathsTo _ Out = 0
pathsTo target (Node name kids)
  | target == name = 1
  | otherwise = sum $ map (pathsTo target) kids

main = do
  myFile <- readFile "exdata11.txt"
  let myData = sort $ map processLine $ lines myFile

  pp myData

  let myGraph = buildGraph (Node "svr" []) myData

  print $ myGraph

  print $ pathsTo "dac" myGraph
