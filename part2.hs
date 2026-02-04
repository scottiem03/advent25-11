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

lookupKids :: (Ord a,Eq b,Eq c) => a -> [(a,b,c)] -> Maybe b
lookupKids target list
  | target == midPoint = Just midCargo
  | firstHalf == [] = Nothing
  | target < midPoint = lookupKids target firstHalf
  | target > midPoint = lookupKids target secondHalf
  where
    midIndex = div (length list) 2
    (midPoint,midCargo,midVisited) = list !! midIndex
    (firstHalf,secondHalf) = splitAt midIndex list

lookupVisit :: (Ord a,Eq b,Eq c) => a -> [(a,b,c)] -> Maybe c
lookupVisit target list
  | target == midPoint = Just midVisited
  | firstHalf == [] = Nothing
  | target < midPoint = lookupVisit target firstHalf
  | target > midPoint = lookupVisit target secondHalf
  where
    midIndex = div (length list) 2
    (midPoint,midCargo,midVisited) = list !! midIndex
    (firstHalf,secondHalf) = splitAt midIndex list

first :: (a,b,c) -> a
first (x,_,_) = x

second :: (a,b,c) -> b
second (_,x,_) = x

third :: (a,b,c) -> c
third (_,_,x) = x

fromMaybeK :: Maybe [a] -> [a]
fromMaybeK Nothing = []
fromMaybeK (Just x) = x

fromMaybeV :: Maybe Visit -> Visit
fromMaybeV Nothing = Paths 0
fromMaybeV (Just x) = x

-- graph-specific code

type Node = (String,[String],Visit)
data Visit = Unex | Paths Int
  deriving (Eq,Show)

unexplored :: String -> [Node] -> Bool
unexplored name myNodes = (getVisit name myNodes) == Unex

explored :: String -> [Node] -> Bool
explored name myNodes = (getVisit name myNodes) /= Unex

sumNodes :: [String] -> [Node] -> Int
sumNodes [] _ = 0
sumNodes (k:ks) myNodes
  | myVisit == Unex = error "can't sum unexplored node"
  | otherwise = thisNum + sumNodes ks myNodes
  where
    myVisit = getVisit k myNodes
    thisNum = fromPaths myVisit

sumKids :: String -> [Node] -> Int
sumKids name myNodes = sumNodes (getKids name myNodes) myNodes

fromPaths :: Visit -> Int
fromPaths Unex = 0
fromPaths (Paths n) = n

processLine :: String -> (String,[String],Visit)
processLine s = (name,kids,visited) where
  myTokens = words s
  name = (init . head) myTokens
  kids = tail myTokens
  visited = Unex

getKids :: String -> [Node] -> [String]
getKids target myNodes = fromMaybeK $ lookupKids target myNodes

noKids :: String -> [Node] -> Bool
noKids name myNodes = length (getKids name myNodes) == 0

getVisit :: String -> [Node] -> Visit
getVisit target myNodes = fromMaybeV $ lookupVisit target myNodes

justUnexKids :: String -> [Node] -> [String]
justUnexKids parent myNodes = filter (\x -> unexplored x myNodes ) candidates where
  candidates = getKids parent myNodes

pathsBetween :: String -> String -> [Node] -> [Node]
pathsBetween start finish myNodes
-- if this is the finish node, Paths = 1
  | start == finish = markPaths start 1 myNodes
-- if this is the end of a branch, Paths = 0
  | noKids start myNodes = markPaths start 0 myNodes
-- if all the kids are explored, Paths = sum of the kids' Paths
  | all (\x -> explored x myNodes) (getKids start myNodes) = markPaths start (sumKids start myNodes) myNodes
-- otherwise, check one of the unexplored kids and try again with updated Graph
  | otherwise = pathsBetween start finish (pathsBetween (head $ justUnexKids start myNodes) finish myNodes)

numPathsBetween :: String -> String -> [Node] -> Int
numPathsBetween start finish myNodes = mySum where
  mySum = fromPaths $ getVisit start $ pathsBetween start finish myNodes

markPaths :: String -> Int -> [Node] -> [Node]
markPaths _ _ [] = []
markPaths name value ((thisName,theseKids,thisVisit):ns)
  | name == thisName = (thisName,theseKids,Paths value) : ns
  | otherwise = (thisName,theseKids,thisVisit) : markPaths name value ns

main = do
  myFile <- readFile "data11.txt"
  let myData = sortOn first $ (("out",[],Unex):(map processLine $ lines myFile))

  -- pp $ myData

  let factor1 = numPathsBetween "dac" "out" myData
  let factor2 = numPathsBetween "fft" "dac" myData
  let factor3 = numPathsBetween "svr" "fft" myData

  print $ factor1 * factor2 * factor3
